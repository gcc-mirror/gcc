/*
 * Copyright (c) 2021-2026 Symas Corporation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following disclaimer
 *   in the documentation and/or other materials provided with the
 *   distribution.
 * * Neither the name of the Symas Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "cobol-system.h"

#include "coretypes.h"
#include "tree.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "diagnostic-core.h"
#include "target.h"

#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "gengen.h"
#include "inspect.h"
#include "../../libgcobol/io.h"
#include "genapi.h"
#include "genutil.h"
#include "genmath.h"
#include "structs.h"
#include "../../libgcobol/gcobolio.h"
#include "../../libgcobol/cobol-endian.h"
#include "../../libgcobol/charmaps.h"
#include "../../libgcobol/valconv.h"
#include "show_parse.h"
#include "fold-const.h"
#include "realmpfr.h"
#include "compare.h"

extern int yylineno;

#define TSI_BACK (tsi_last(current_function->statement_list_stack.back()))

extern char *cobol_name_mangler(const char *cobol_name);
static tree label_list_out_goto;
static tree label_list_out_label;
static tree label_list_back_goto;
static tree label_list_back_label;

#ifdef ENABLE_HIJACKING
//#pragma message "HIJACKING IS ENABLED - It should be disabled for release"
static bool hijacked = false;  // Indicates a DUBNER hijacking is in progress.
static void hijack_for_development(const char *funcname);
static void hijacker();
#define RETURN_WHEN_HIJACKED do{if(hijacked){return;}}while(0);
#else
#define RETURN_WHEN_HIJACKED
#define hijacked (false)
#endif

static size_t sv_data_name_counter = 1;

static bool suppress_cobol_entry_point = false;
static char ach_cobol_entry_point[256] = "";

bool bSHOW_PARSE = getenv("GCOBOL_SHOW");
bool show_parse_sol = true;
int  show_parse_indent = 0;

static bool sv_is_i_o = false;

static int perform_is_armed = 0;
static std::map<int, int> perform_line_pairs;

#ifdef LINE_TICK
/*  This code is used from time to time when sorting out why compilation
    takes more time than expected */
static void
line_tick()
  {
  using namespace std::chrono;
  static high_resolution_clock::time_point t1 = high_resolution_clock::now();
  static high_resolution_clock::time_point t2;
  int line_now = CURRENT_LINE_NUMBER;
  static int line = 0;
  if( (line_now / 10000) != (line / 10000) )
    {
    line = line_now;
    t2 = high_resolution_clock::now();
    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);
    fprintf(stderr, "%6d %6.1lf\n", line, time_span.count());
    }
  }
#else
#define line_tick()
#endif

// set using -f-trace-debug, defined in lang.opt
int f_trace_debug;

// When doing WRITE statements, the IBM Language Reference and the
// ISO/IEC_2014 standard specify that when the ADVANCING clause is omitted, the
// default isAFTER ADVANCING 1 LINE.
//
// MicroFocus and GnuCOBOL state that the default is BEFORE ADVANCING 1 LINE
//
// During initial compiler development, we used Michael Coughlin's "Beginning
// COBOL For Programmers" textbook for source code examples, and it was clear
// from at least one sample program that his compiler used the Microfocus
// convention.  For ease of development, we took on that same convention, but
// we provide here for a switch that changes that behavior:

static bool auto_advance_is_AFTER_advancing = 0;

/*  This is a little complicated.  In order to keep things general, we are
    assuming that any function we call will be returning a 64-bit value.  In
    places where we know that not to be true, we'll have to do appropriate
    casts.  For example, main() returns an INT, as do functions that
    return the default RETURN-CODE will have */

#define COBOL_FUNCTION_RETURN_TYPE SSIZE_T

#define MAX_AFTERS 8

// These variables control a little state machine.  When a simple -main is in
// effect, the first program in the module becomes the target of a main()
// that we synthesize function.  When -main=module:progid is in effect, we
// create a main() that calls progid.  When active, progid is kept in
// the map main_strings.
static std::unordered_map<std::string, std::string> main_strings;
static bool this_module_has_main = false;   // sticky switch for the module
static bool next_program_is_main = false;   // transient switch for the module
static char *main_entry_point = NULL;

static bool static_call = true;
bool use_static_call( bool yn ) { return static_call = yn; }
static bool use_static_call() { return static_call; }

// This global variable can be set upstream, like from a compiler
// command line switch.  "1" for stdout, "2" for stderr, or "filename"

const char *gv_trace_switch = NULL;

// The environment variable wins over the command line
char const *bTRACE1 = NULL;
tree trace_handle;
tree trace_indent;

// This variable is set to true when the output cursor is known to be at the
// start-of-line.
bool cursor_at_sol = true;

static void
trace1_init()
  {
  static bool first_time = true;
  if( first_time )
    {
    first_time = false;
    trace_handle = gg_define_variable(INT, "_trace_handle", vs_static);
    trace_indent = gg_define_variable(INT, "_trace_indent", vs_static);

    bTRACE1 = getenv("GCOBOL_TRACE") ? getenv("GCOBOL_TRACE") :gv_trace_switch;

    if( bTRACE1 && strcmp(bTRACE1, "0") != 0 )
      {
      if( strcmp(bTRACE1, "1") == 0 )
        {
        gg_assign(trace_handle , integer_one_node);
        }
      else if( strcmp(bTRACE1, "2") == 0 )
        {
        gg_assign(trace_handle , integer_two_node);
        }
      else
        {
        gg_assign(trace_handle ,
                  gg_open(gg_string_literal(bTRACE1),
                          build_int_cst_type(INT, O_CREAT|O_WRONLY|O_TRUNC)));
        }
      }
    else
      {
      // In case bTRACE1 pointed to an empty string
      bTRACE1 = NULL;
      }
    }
  }

static
void
insert_nop(int n)
  {
  gg_assign(var_decl_nop, build_int_cst_type(INT, n));
  }

static void
create_cblc_string_variable(const char *var_name, const char *var_contents)
  {
  // This is a way of having the compiler communicate with GDB.  I create a
  // global const char[] string with a known name so that GDB can look for that
  // variable and pick up its contents.

  // This probably should be in the .debug_info section, but for the moment I
  // don't know how to do that, but I do know how to do this:

  tree array_of_characters = build_array_type_nelts(CHAR, strlen(var_contents)+1);
  TYPE_NAME(array_of_characters) = get_identifier("cblc_string");
  tree constr = build_string(strlen(var_contents)+1, var_contents);
  TREE_TYPE(constr) = array_of_characters;
  TREE_STATIC(constr)           = 1;

  // This is a file-scope internal variable
  tree entry_point = gg_declare_variable(array_of_characters,
                                         var_name,
                                         constr,
                                         vs_global);
  gg_define_from_declaration(entry_point);
  }

static void
build_main_that_calls_something(const char *something)
  {
  // This routine generates main(), which has as its body a call to "something".
  // which is a call to a simple `extern int something(void)` routine.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" main will call ")
    SHOW_PARSE_TEXT(something)
    SHOW_PARSE_END
    }

  tree function_decl = gg_define_function( INT,
                                           "main",
                                           "main",
                                           INT, "argc",
                                           build_pointer_type(CHAR_P), "argv",
                                           NULL_TREE);

  // Modify the default settings for main(), as empirically determined from
  // examining C/C+_+ compilations.  (See the comment for gg_build_fn_decl()).
    TREE_ADDRESSABLE(function_decl) = 0;
    TREE_USED(function_decl) = 0;
    TREE_NOTHROW(function_decl) = 0;
    TREE_STATIC(function_decl) = 1;
    DECL_EXTERNAL (function_decl) = 0;
    TREE_PUBLIC (function_decl) = 1;
    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(function_decl) = 1;

  // Pick up pointers to the input parameters:
  // First is the INT which is the number of argv[] entries
  tree argc         = DECL_ARGUMENTS(current_function->function_decl);
  // Second is the char **argv
  tree argv         = TREE_CHAIN(argc);          // overall source length

  gg_call(  VOID,
            "__gg__stash_argc_argv",
            argc,
            argv,
            NULL_TREE);

  // Call the top-level COBOL function.  We know it has to return an INT,
  // so we need to cast it from the SIZE_T that all COBOL are assumed
  // to return:

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("main calls \"", something, "\"")
    TRACE1_END
    }

  // Let MODULE-NAME know that we were launched by a generated -main program
  gg_call(VOID,
          "__gg__module_name_push",
          gg_string_literal("Mmain"),
          NULL_TREE);

  char *psz = cobol_name_mangler(something);
  gg_assign(var_decl_main_called, integer_one_node);
  gg_return(gg_cast(INT, gg_call_expr( COBOL_FUNCTION_RETURN_TYPE,
                                       psz,
                                       argc,
                                       argv,
                                       NULL_TREE)));
  free(psz);
  gg_finalize_function();
  }

static std::unordered_map<std::string, size_t>gotos_labels;
#define LABEL_COUNT_OFFSET 100

static
tree
get_field_p(size_t index)
  {
  if(index)
    {
    cbl_field_t *field = cbl_field_of(symbol_at(index));

    if( !field->var_decl_node )
      {
      dbgmsg("%s (type: %s) improperly has a NULL var_decl_node",
                  field->name,
                  cbl_field_type_str(field->type));
      cbl_internal_error(
                "Probable cause: it was referenced without being defined.");
      }

    return gg_get_address_of(field->var_decl_node);
    }
  else
    {
    return gg_cast(cblc_field_p_type_node, null_pointer_node);
    }
  }

static
char *
level_88_helper(size_t parent_capacity,
                const cbl_domain_elem_t &elem,
                size_t &returned_size)
  {
  // We return a MALLOCed return value, which the caller must free.
  char *retval  = static_cast<char *>(xmalloc(parent_capacity + 64));
  gcc_assert(retval);
  char *builder = static_cast<char *>(xmalloc(parent_capacity + 64));
  gcc_assert(builder);

  size_t nbuild = 0;

  cbl_figconst_t figconst = cbl_figconst_of( elem.name());
  if( figconst )
    {
    nbuild = 1;
    strcpy(retval, "1Fx");
    switch(figconst)
      {
      case normal_value_e :
        // This really should never happen
        abort();
        break;
      case low_value_e    :
        retval[2] = 'L';
        break;
      case zero_value_e   :
        retval[2] = 'Z';
        break;
      case space_value_e  :
        retval[2] = 'S';
        break;
      case quote_value_e  :
        retval[2] = 'Q';
        break;
      case high_value_e   :
        retval[2] = 'H';
        break;
      case null_value_e:
        retval[2] = '\0';
        break;
      }
    returned_size = 3;
    }
  else
    {
    // We are working with an ordinary string.

    // Pick up the string
    size_t first_name_length  = elem.size();
    char *first_name = static_cast<char *>(xmalloc(first_name_length + 1));
    gcc_assert(first_name);
    memcpy(first_name, elem.name(), first_name_length);
    first_name[first_name_length] = '\0';

    if( parent_capacity == 0 )
      {
      // Special case:  parent_capacity is zero when this routine has been
      // called as part of a debugging trace.
      if( elem.all )
        {
        strcpy(builder+nbuild, "ALL ");
        nbuild += 4;
        }
      memcpy(builder+nbuild, first_name, first_name_length);
      nbuild += first_name_length;
      }
    else
      {
      if( elem.all )
        {
        while(nbuild < parent_capacity )
          {
          builder[nbuild] = first_name[nbuild % first_name_length];
          nbuild += 1;
          }
        }
      else
        {
        memcpy(builder+nbuild, first_name, first_name_length);
        nbuild += first_name_length;
        }
      }
    returned_size = sprintf(retval, HOST_SIZE_T_PRINT_DEC "A",
                            (fmt_size_t)nbuild);
    memcpy(retval + returned_size, builder, nbuild);
    returned_size += nbuild;
    free(first_name);
    free(builder);
    }
  return retval;
  }

static char *
get_level_88_domain(size_t parent_capacity, cbl_field_t *var, size_t &returned_size)
  {
  if( var->type != FldClass || var->level != 88 )
    {
    returned_size = 0;
    return NULL;
    }

  // Entering here means we know that this is FldClass of level 88

  // We convert the incoming information at var->data.domains to a single
  // stream of bytes.  We return a malloced pointer to that stream; returned
  // size is the size of the stream.

  // The nature of an 88 is that each element is a pair

  // The following pairs are zero-terminated strings.  It thus
  // follows that the strings cannot contain '\0' characters.

  // Each element of the pair is converted to a stream:
  // For strings of bytes:
  //       ddd A <ddd bytes>
  // For figurative constants:
  //       1Fx, where x is in [LZSQH], for LOW-VALUE ZERO SPACE QUOTE HIGH-VALUE

  // Numerics are converted to strings, and handled as above

  /*  For example:

       77 var-1 PIC 99V9.
           88 var-1-z VALUE zero THRU 10.
           88 var-1-big VALUE 20 THRU 40.
           88 var-1-huge VALUE 40 THRU 999.
           88 var-1-asc VALUE "U2" THRU "XYZZY".

    Creates these four string segments:

      "1FZ2A10"
      "2A202A40"
      "2A403A999"
      "2AU25AXYZZY"

    Each gets converted to UTF-32 as the initial value.

    */

  size_t retval_capacity = 64;
  char *retval = static_cast<char *>(xmalloc(retval_capacity));
  size_t output_index = 0;

  // Loop through the provided domains:
  returned_size = 0;
  const struct cbl_domain_t *domain = var->data.domain_of();
  while( domain->first.name() )
    {
    // We have another pair to process
    size_t stream_len;
    char *stream;

    // Do the first element of the domain
    stream = level_88_helper( parent_capacity,
                              domain->first,
                              stream_len);
    if( output_index + stream_len > retval_capacity )
      {
      retval_capacity *= 2;
      retval = static_cast<char *>(xrealloc(retval, retval_capacity));
      }
    gcc_assert(retval);
    memcpy(retval + output_index, stream, stream_len);
    output_index += stream_len;
    returned_size += stream_len;
    free(stream);

    // Do the second element of the domain
    stream = level_88_helper( parent_capacity,
                              domain->last,
                              stream_len);
    if( output_index + stream_len > retval_capacity )
      {
      retval_capacity *= 2;
      retval = static_cast<char *>(xrealloc(retval, retval_capacity));
      }
    gcc_assert(retval);
    memcpy(retval + output_index, stream, stream_len);
    output_index += stream_len;
    returned_size += stream_len;
    free(stream);
    domain += 1;
    }

  if( returned_size >= retval_capacity)
    {
    retval_capacity *= 2;
    retval = static_cast<char *>(xrealloc(retval, retval_capacity));
    }

  gcc_assert(returned_size < retval_capacity);
  retval[returned_size++] = '\0';
  return retval;
  }

static
char *
get_class_condition_string(cbl_field_t *var)
  {
  // This routine returns a malloced pointer.

  // We know at this point that var is FldClass
  // The LEVEL is not 88, so this is a CLASS SPECIAL-NAME

  const struct cbl_domain_t *domain = var->data.domain_of();

  /*  There are five possibilities we need to deal with.

      66
      66 THROUGH 91
      91 THROUGH 66   // This is the same as 66 THROUGH 91
      "A"
      "A" THROUGH "Z
      "Z" THROUGH "A" // This is the same as "A" THROUGH "Z"
      "ABCJ12"        // This is the same as "A" "B" "C" ...

      Expressly presented numbers are the ordinal positions in the run-time
      character set. We encode those values with a leading ascii_hyphen to
      distinguish them from characters.

      Characters are converted to UTF32 values, and then encoded as big-endian
      hexadecimal characters.

      A range of values is encoded as a pair of hexadecimal values with an
      ascii_slash between them.  The second value ends with a space

      A list of characters is encoded simply as a stream of hexadecimal values
      separated by spaces.
      */

  char ach[8192];
  memset(ach, 0, sizeof(ach));
  char *p = ach;

  while( domain->first.is_numeric || domain->first.name() )
    {
    size_t first_name_length = domain->first.size()
                              ? domain->first.size()
                              : strlen(domain->first.name());

    cbl_encoding_t from = var->codeset.default_encodings.source->type;
    cbl_encoding_t to = HOST_32_ENCODING;
    size_t nbytes;
    const char *converted;

    if( domain->first.is_numeric )
      {
      if( strlen(ach) > sizeof(ach) - 1000  )
        {
        cbl_internal_error("That string should not be that long.");
        }

      // We are working with unquoted strings that contain the values
      uint32_t value1 = atoll(domain->first.name());
      uint32_t value2 = atoll(domain->last.name());
      if( value2 < value1 )
        {
        std::swap(value1, value2);
        }
      if( value1 != value2  )
        {
        p += sprintf(p, "-%X/-%X ", value1, value2);
        }
      else
        {
        p += sprintf(p, "-%X ", value1);
        }
      }
    else if( first_name_length == 1 )
      {
      // Since the first.name is a single character, we can do this as
      // a single-character pair.
      converted = __gg__iconverter(from,
                                   to,
                                   domain->first.name(),
                                   1,
                                   &nbytes);
      cbl_char_t ch1 = *reinterpret_cast<const cbl_char_t *>(converted);
      converted = __gg__iconverter(from,
                                   to,
                                   domain->last.name(),
                                   1,
                                   &nbytes);
      cbl_char_t ch2 = *reinterpret_cast<const cbl_char_t *>(converted);

      if( ch1 > ch2 )
        {
        std::swap(ch1, ch2);
        }
      if( ch1 != ch2  )
        {
        p += sprintf(p, "%X/%X ", ch1, ch2);
        }
      else
        {
        p += sprintf(p, "%X ", ch1);
        }
      }
    else
      {
      gcc_assert( first_name_length > 1 );

      // We are working with a string larger than 1 character.  The COBOL
      // spec says there can't be a THROUGH, so we ignore the last.name:
      // size_t first_name_length = domain->first.size()
                                // ? domain->first.size()
                                // : strlen(domain->first.name());
      for(size_t i=0; i<first_name_length; i++)
        {
        converted = __gg__iconverter(from,
                                     to,
                                     domain->first.name()+i,
                                     1,
                                     &nbytes);
        cbl_char_t ch1 = *reinterpret_cast<const cbl_char_t *>(converted);
        p += sprintf(p, "%X ", ch1);
        }
      }
    domain += 1;
    }

  // Wipe out the trailing space
  ach[strlen(ach)-1] = '\0';
  char *retval = xstrdup(ach);

  return retval;
  }

struct program_reference_t {
  size_t caller;
  const char *called;

  program_reference_t( size_t caller, const char called[] )
    : caller(caller), called(xstrdup(called))
  {}
  bool operator==( const program_reference_t& that ) const {
    return caller == that.caller && 0 == strcasecmp(called, that.called);
  }
  bool operator<( const program_reference_t& that ) const {
    if( caller == that.caller ) return 0 < strcasecmp(called, that.called);
    return caller < that.caller;
  }
};

struct called_tree_t {
  tree node;
  cbl_call_convention_t convention;

  called_tree_t( tree node,
                 cbl_call_convention_t convention )
    : node(node), convention(convention)
  {}
  bool operator==( const called_tree_t& that ) const {
    return node == that.node && convention == that.convention;
  }

  class match_tree {  // match node regardless of convention
    tree node;

   public:
    explicit match_tree( tree node ) : node(node) {}
    bool operator()( const called_tree_t& that ) const {
      return this->node == that.node;
    }
  };
};

static std::map<program_reference_t, std::list<tree> > call_targets;
static std::map<tree, cbl_call_convention_t> called_targets;

static
void
set_call_convention(tree function_decl, cbl_call_convention_t convention)
  {
  called_targets[function_decl] = convention;
  }

static
void
parser_call_target( const char *name, tree call_expr )
  {
  /*  This routine gets called when parser_call() has been invoked with a
      literal target.  That target is a COBOL name like "prog_2".  However,
      there is the case when "prog_2" is a forward reference to a contained
      program nested inside "prog_1".  In that case, the actual definition
      of "prog_2" will end up with a name like "prog_2.62", and eventually
      the target of the call will have to be modified from "prog_2" to
      "prog_2.62".

      We save the call expression for this call, and then we update it later,
      after we know whether or not it was a forward reference to a local
      function. */

  program_reference_t key(current_program_index(), name);
  auto& p = call_targets[key];
  p.push_back(call_expr);
  }

/*
 * Is the node a recorded call target?  The language-dependent
 * function cobol_set_decl_assembler_name will lower-case the name
 * unless, for a specific call, this function returns
 * cbl_call_verbatim_e.
 */
cbl_call_convention_t
parser_call_target_convention( tree func )
  {
  auto p = called_targets.find(func);
  if( p != called_targets.end() )
    {
    // This was found in our list of call targets
    return p->second;
    }

  return cbl_call_cobol_e;
  }

size_t
parser_call_target_update( size_t caller,
                           const char plain_name[],
                           const char mangled_name[] )
  {
  auto key = program_reference_t(caller, plain_name);
  auto p = call_targets.find(key);
  if( p == call_targets.end() ) return 0;

  for( auto call_expr : p->second )
    {
    tree fndecl_type = build_varargs_function_type_array( COBOL_FUNCTION_RETURN_TYPE,
                       0,     // No parameters yet
                       NULL); // And, hence, no types

    // Fetch the FUNCTION_DECL for that FUNCTION_TYPE
    tree function_decl = gg_build_fn_decl(mangled_name, fndecl_type);
    tree function_address = gg_get_address_of(function_decl);

    TREE_OPERAND(call_expr, 1) = function_address;
    }
  return p->second.size();
  }

static tree
function_pointer_from_name(const cbl_refer_t &name,
                           tree function_return_type)
  {
  Analyze();

  tree function_type = build_varargs_function_type_array(
                        function_return_type,
                        0,
                        NULL);
  tree function_pointer_type = build_pointer_type(function_type);
  tree function_pointer       = gg_define_variable(function_pointer_type,
                                                  "..function_pointer.1",
                                                  vs_stack);
  if( name.field->type == FldPointer )
    {
    // If the parameter is a pointer, just pick up the value and head for the
    // exit
    if( refer_is_clean(name) )
      {
      gg_memcpy(gg_get_address_of(function_pointer),
                member(name.field->var_decl_node, "data"),
                sizeof_pointer);
      }
    else
      {
      gg_memcpy(gg_get_address_of(function_pointer),
                qualified_data_location(name),
                sizeof_pointer);
      }
    return function_pointer;
    }
  else if( use_static_call() && is_literal(name.field) )
    {
    tree fndecl_type = build_varargs_function_type_array( function_return_type,
                       0,     // No parameters yet
                       NULL); // And, hence, no types

    // Fetch the FUNCTION_DECL for that FUNCTION_TYPE
    char *tname = static_cast<char *>(xmalloc(name.field->data.capacity()+1));
    memcpy(tname, name.field->data.original(), name.field->data.capacity());
    tname[name.field->data.capacity()] = '\0';
    tree function_decl = gg_build_fn_decl(tname,
                                          fndecl_type);
    free(tname);
    // Take the address of the function decl:
    tree address_of_function = gg_get_address_of(function_decl);
    gg_assign(function_pointer, address_of_function);
    }
  else
    {
    // We are not using static calls.
    if( name.field->type == FldLiteralA )
      {
      gg_assign(function_pointer,
                gg_cast(build_pointer_type(function_type),
                        gg_call_expr( VOID_P,
                              "__gg__function_handle_from_literal",
                              build_int_cst_type(INT,
                                current_function->our_symbol_table_index),
                              gg_string_literal(name.field->data.original()),
                              NULL_TREE)));
      }
    else
      {
      gg_assign(function_pointer,
                gg_cast(build_pointer_type(function_type),
                        gg_call_expr( VOID_P,
                                "__gg__function_handle_from_name",
                                build_int_cst_type(INT,
                                current_function->our_symbol_table_index),
                                gg_get_address_of(name.field->var_decl_node),
                                refer_offset(name),
                                refer_size_source(  name),
                                NULL_TREE)));
      }
    }

  return function_pointer;
  }

void
parser_initialize_programs( size_t nprogs,
                            const struct cbl_refer_t *progs)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    for( size_t i=0; i<nprogs; i++)
      {
      if( i > 0 )
        {
        SHOW_PARSE_INDENT
        }
      if( progs[i].field->type == FldLiteralA )
        {
        SHOW_PARSE_TEXT("\"")
        SHOW_PARSE_TEXT(progs[i].field->data.original())
        SHOW_PARSE_TEXT("\"")
        }
      else
        {
        SHOW_PARSE_TEXT("")
        SHOW_PARSE_TEXT(progs[i].field->name)
        }
      }
    SHOW_PARSE_END
    }

  for( size_t i=0; i<nprogs; i++ )
    {
    tree function_pointer = function_pointer_from_name( progs[i],
                                                        COBOL_FUNCTION_RETURN_TYPE);
    gg_call(VOID,
            "__gg__to_be_canceled",
            function_pointer,
            NULL_TREE);
    }
  }

static tree
array_of_uint64(const char *name,
                   const std::vector<uint64_t> &vals)
  {
  /*
   * Create:
   *
   *   static const unsigned long long name[] =
   *     {
   *     vals.size(),
   *     vals[0],
   *     vals[1],
   *     ...
   *     };
   */
  tree const_uint64_type =
    build_qualified_type( UINT64,
                          TYPE_QUAL_CONST );
  tree array_of_uint64_type =
    build_array_type_nelts( const_uint64_type,
                            vals.size()+1 );
  tree array_of_uint64 =
    gg_define_variable( array_of_uint64_type,
                        name,
                        vs_file_static );
  vec<constructor_elt, va_gc> *elts = NULL;
  /*
   * The first element contains the number of elements that follow.
   */
  CONSTRUCTOR_APPEND_ELT(
    elts,
    bitsize_int( 0 ),
    build_int_cstu( UINT64, vals.size() ) );

  for( size_t i=0; i<vals.size(); i++ )
    {
    CONSTRUCTOR_APPEND_ELT(
      elts,
      bitsize_int( i+1 ),
      build_int_cstu( UINT64, vals[i] ) );
    }
  tree constr =
    build_constructor( array_of_uint64_type,
                       elts );
  /*
   * build_constructor() determines TREE_CONSTANT from its elements.
   * All of these elements are INTEGER_CST nodes.
   */
  gcc_assert( TREE_CONSTANT( constr ) );
  /*
   * The constructor represents a value suitable for static storage.
   */
  TREE_STATIC( constr ) = 1;
  /*
   * Record the const qualification on the declaration itself.
   */
  TREE_READONLY( array_of_uint64 ) = 1;
  DECL_INITIAL( array_of_uint64 ) = constr;
  return array_of_uint64;
  }

tree
gg_array_of_size_t(const std::vector<size_t> &values)
  {
  gcc_assert( !values.empty() );
  tree const_size_t_type = build_qualified_type( SIZE_T, TYPE_QUAL_CONST );
  tree array_type = build_array_type_nelts( const_size_t_type, values.size() );

  vec<constructor_elt, va_gc> *elts = NULL;
  for( size_t i = 0; i < values.size(); i++ )
    {
    CONSTRUCTOR_APPEND_ELT(
      elts,
      bitsize_int( i ),
      build_int_cstu( SIZE_T, values[i] ) );
    }
  tree constr = build_constructor( array_type, elts );
  /*
   * This marks the constant constructor as suitable for static
   * allocation.  It does not give the VAR_DECL static storage
   * duration.
   */
  TREE_STATIC( constr ) = 1;
  tree array_decl = gg_define_variable( array_type );
  /*
   * Represent the const qualification on the object as well as on
   * its array element type.
   */
  TREE_READONLY( array_decl ) = 1;
  DECL_INITIAL( array_decl ) = constr;
  return gg_pointer_to_array(array_decl);
  }

/*
 * As ECs are enabled and disabled with >>TURN, the compiler updates its list
 * of enabled ECs (and any files they apply to). It encodes this list as an
 * array of integers.  parser_compile_ecs converts that array as a static
 * compile-time vector, which it returns to the compiler.
 *
 * Before each statement, the compiler determines what possible EC handling the
 * program can do.  If there's an overlap between potential ECs and
 * Declaratives, it passes the current pair of static arrays to
 * parser_statement_begin(), which installs them, for that statement, in the
 * library.
 *
 * After each statement, to effect EC handling, the statement epilog calls uses
 * parser_match_exception to invoke __gg_match_exception(), which returns the
 * symbol table index of the matched Declarative, if any.  That "ladder"
 * Performs the matched declarative, and execution continues with the next
 * statement.
 */
tree
parser_compile_ecs( const std::vector<uint64_t>& ecs )
  {
  if( ecs.empty() )
    {
    SHOW_IF_PARSE(nullptr)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_TEXT("ecs is empty");
      SHOW_PARSE_END
      }
    return NULL_TREE;
    }

  char ach[64];
  static int counter = 1;
  sprintf(ach, "_ecs_table_%d", counter++);
  tree retval =  array_of_uint64(ach, ecs);
  SHOW_IF_PARSE(nullptr)
    {
    SHOW_PARSE_HEADER
    snprintf(ach, sizeof(ach), " Size is %lu; retval is %p",
             gb4(ecs.size()), as_voidp(retval));
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    snprintf(ach, sizeof(ach), " Size is %lu; retval is %p",
             gb4(ecs.size()), as_voidp(retval));
    TRACE1_TEXT_ABC("", ach, "");
    TRACE1_END
    }
  return retval;
  }

/*
 * At the beginning of Procedure Division, we may encounter DECLARATIVES
 * SECTION.  If so, the compiler composes a list of zero or more Declaratives
 * as cbl_declarative_t, representing the USE statement of each
 * Declarative. These are encoded as an array of integers, which are returned
 * to the compiler for use by parser_statement_begin(). Although the list of
 * declaratives never changes for a program, CALL may change which program is
 * invoked, and thus the set of active Declaratives.  By passing them for each
 * statement, code generation is relieved of referring to global variable.
 */
tree
parser_compile_dcls( const std::vector<uint64_t>& dcls )
  {
  if( dcls.empty() )
    {
    SHOW_IF_PARSE(nullptr)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_TEXT("dcls is empty");
      SHOW_PARSE_END
      }
    return NULL_TREE;
    }

  char ach[64];
  static int counter = 1;
  sprintf(ach, "_dcls_table_%d", counter++);
  tree retval =  array_of_uint64(ach, dcls);
  SHOW_IF_PARSE(nullptr)
    {
    SHOW_PARSE_HEADER
    snprintf(ach, sizeof(ach), " Size is %lu; retval is %p",
             gb4(dcls.size()), as_voidp(retval));
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    snprintf(ach, sizeof(ach), " Size is %lu; retval is %p",
             gb4(dcls.size()), as_voidp(retval));
    TRACE1_TEXT_ABC("", ach, "");
    TRACE1_END
    }
  return retval;
  }

static void
store_location_stuff(const cbl_name_t statement_name)
  {
  if( exception_location_active && !current_declarative_section_name() )
    {
    // We need to establish some stuff for EXCEPTION- function processing

    gg_assign(var_decl_exception_program_id,
              gg_string_literal(current_function->our_unmangled_name));

    if( strstr(current_function->current_section->label->name, "_implicit")
        != current_function->current_section->label->name )
      {
      gg_assign(var_decl_exception_section,
           gg_string_literal(current_function->current_section->label->name));
      }
    else
      {
      gg_assign(var_decl_exception_section,
                gg_cast(build_pointer_type(CHAR_P),null_pointer_node));
      }

    if( strstr(current_function->current_paragraph->label->name, "_implicit")
        != current_function->current_paragraph->label->name )
      {
      gg_assign(var_decl_exception_paragraph,
           gg_string_literal(current_function->current_paragraph->label->name));
      }
    else
      {
      gg_assign(var_decl_exception_paragraph,
                gg_cast(build_pointer_type(CHAR_P), null_pointer_node));
      }

    gg_assign(var_decl_exception_source_file,
              gg_string_literal(current_filename.back().c_str()));
    gg_assign(var_decl_exception_line_number, build_int_cst_type(INT,
                                                          CURRENT_LINE_NUMBER));
    gg_assign(var_decl_exception_statement, gg_string_literal(statement_name));
    }
  }

static
void
set_exception_environment( tree ecs, tree dcls )
  {
  gg_call(VOID,
          "__gg__set_exception_environment",
          ecs  ? gg_pointer_to_array(ecs) : null_pointer_node,
          dcls ? gg_pointer_to_array(dcls) : null_pointer_node,
          NULL_TREE);
  }

void
parser_statement_begin( const cbl_name_t statement_name,
                        tree ecs,
                        tree dcls )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[64];
    snprintf( ach, sizeof(ach),
              " yylineno %d first/last %d/%d",
              yylineno,
              cobol_location().first_line,
              cobol_location().last_line );
    SHOW_PARSE_TEXT(ach);
    if( true || ecs || dcls )
      {
      SHOW_PARSE_INDENT
      snprintf( ach, sizeof(ach),
                "Sending ecs/dcls %p / %p", as_voidp(ecs), as_voidp(dcls));
      SHOW_PARSE_TEXT(ach);
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    char ach[64];
    snprintf(ach, sizeof(ach), " ecs/dcls %p / %p", as_voidp(ecs), as_voidp(dcls));
    TRACE1_TEXT_ABC("", ach, "");
    TRACE1_END
    }

  gcc_assert( gg_trans_unit.function_stack.size() );

  // If a PERFORM is armed, that's the line that the PERFORM is on.  The
  // cobol_location().first_line here is the major statement following the
  // the PERFORM statement.  (We don't use .loc information in GDB because of
  // the difficulty in teasing out which is the "primary" .loc from the
  // 'is_stmt' and 'discriminator'.  If that's possible, I haven't yet figured
  // how.)
  if( perform_is_armed )
    {
    perform_line_pairs[perform_is_armed] = cobol_location().first_line;
    perform_is_armed = 0;
    }

  // In the cases where enabled_exceptions.size() is non-zero, or when
  // there is a possibility of an EC-I-O exception because this is a file
  // operation, we need to store the location information and do the exception
  // overhead:

  static const std::set<std::string> file_ops =
    {
    "OPEN",
    "CLOSE",
    "READ",
    "WRITE",
    "DELETE",
    "REWRITE",
    "START",
    };

  //  Performance note:  By doing exception processing only when necessary
  //  the execution time of a program doing two-billion simple adds in an inner
  //  loop dropped from 3.8 seconds to 0.175 seconds.

  bool exception_processing = cdf_enabled_exceptions().size() ;

  if( !exception_processing )
    {
    exception_processing = file_ops.find(statement_name) != file_ops.end();
    }

  // At this point, if any exception is enabled, we store the location stuff.
  // Each file I-O routine calls store_location_stuff explicitly, because
  // those exceptions can't be defeated.

  if( exception_processing )
    {
    store_location_stuff(statement_name);
    set_exception_environment(ecs, dcls);
    }

  sv_is_i_o = false;
  }

void
parser_statement_end( const std::list<cbl_field_t*>&flist)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char *psz = xasprintf(" List has %ld elements", flist.size());
    SHOW_PARSE_TEXT(psz);
    free(psz);
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    char *psz = xasprintf(" List has %ld elements", flist.size());
    TRACE1_TEXT(psz);
    free(psz);
    TRACE1_END
    }
  if( flist.size() )
    {
    for( auto field : flist )
      {
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char *psz = xasprintf("Deallocating %s", field->name);
        SHOW_PARSE_TEXT(psz);
        free(psz);
        }
      TRACE1
        {
        TRACE1_INDENT
        char *psz = xasprintf(" Deallocating %s", field->name);
        TRACE1_TEXT(psz);
        free(psz);
        }

      gg_free(member(field->var_decl_node, "data"));
      // Flag this guy as free:
      gg_assign(member(field->var_decl_node, "data"), gg_cast(UCHAR_P, null_pointer_node));
      gg_assign(member(field->var_decl_node, "allocated"), gg_cast(SIZE_T, integer_zero_node));
      }
    TRACE1
      {
      TRACE1_END
      }
    }
  }


static const int DEFAULT_BYTE_MASK = 0x00000000FF;
static const int NSUBSCRIPT_MASK   = 0x0000000F00;
static const int NSUBSCRIPT_SHIFT  =            8;
static const int DEFAULTBYTE_BIT   = 0x0000001000;
static const int EXPLICIT_BIT      = 0x0000002000;
static const int REDEFINED_BIT     = 0x0000004000;
static const int JUST_ONCE_BIT     = 0x0000008000;

static void
initialize_variable_internal( cbl_refer_t refer,
                              bool explicitly=false,
                              bool just_once=false)
  {
  cbl_field_t *parsed_var = refer.field;
  if( !parsed_var )
    {
    cbl_internal_error("%s should not be null", "parsed_var");
    }

  if( parsed_var->is_key_name() )
    {
    // This field is actually a placeholder for a RECORD KEY alias.  It didn't
    // go through parser_symbol_add(), and so any attempt to initialize it
    // results in an error because there is no var_decl_node.
    return;
    }

  if(     parsed_var->attr & register_e
      || (   parsed_var->attr & intermediate_e
          && parsed_var->type == FldAlphanumeric) )
    {
    return;
    }

  Analyze();
  SHOW_PARSE
    {
    do
      {
      fprintf(  stderr,
                "( %d ) %s():",
                CURRENT_LINE_NUMBER,
                __func__);
      }
    while(0);
    SHOW_PARSE_REF(" ", refer);
    if( parsed_var->data.original() )
      {
      SHOW_PARSE_TEXT(" >>")
      if( parsed_var->level == 88)
        {
        size_t returned_size = 0;
        char *string88 = get_level_88_domain(0, parsed_var, returned_size);

        char *p = string88;
        bool first = true;
        while(*p)
          {
          char *pend;
          size_t length1 = strtoull(p, &pend, 10);
          char *string1  = pend + 1;
          char flag = *pend;
          p = string1 + length1;
          if(flag == 'A' )
            {
            char ach2[] = "x";
            SHOW_PARSE_TEXT("\"")
            for(size_t i=0; i<length1; i++)
              {
              ach2[0] = string1[i];
              SHOW_PARSE_TEXT(ach2)
              }
            SHOW_PARSE_TEXT("\"")
            }
          else
            {
            switch(string1[0])
              {
              case 'L':
                SHOW_PARSE_TEXT("LOW-VALUE")
                break;
              case 'Z':
                SHOW_PARSE_TEXT("ZERO")
                break;
              case 'S':
                SHOW_PARSE_TEXT("SPACE")
                break;
              case 'Q':
                SHOW_PARSE_TEXT("QUOTE")
                break;
              case 'H':
                SHOW_PARSE_TEXT("HIGH-VALUE")
                break;
              default:
                SHOW_PARSE_TEXT("???")
                break;
              }
            }
          if( first )
            {
            SHOW_PARSE_TEXT("/")
            }
          else
            {
            if(*p)
              {
              SHOW_PARSE_TEXT(" ")
              }
            }
          first = !first;
          }
        free(string88);
        }
      else if( parsed_var->type == FldClass )
        {
        char *p = get_class_condition_string(parsed_var);
        SHOW_PARSE_TEXT(p);
        free(p);
        }
      else
        {
        switch(parsed_var->type)
          {
          case FldGroup:
          case FldAlphanumeric:
          case FldNumericEdited:
          case FldAlphaEdited:
          case FldLiteralA:
            SHOW_PARSE_TEXT(parsed_var->data.original());
            break;
          default:
            {
            char ach[128];
            if( TREE_CODE(TREE_TYPE(parsed_var->data.value_of())) == REAL_TYPE)
              {
              real_to_decimal (ach,
                               TREE_REAL_CST_PTR (parsed_var->data.value_of()),
                               sizeof(ach), 16, 0);
              }
            else
              {
              wi::tree_to_wide_ref iii =
                                    wi::to_wide( parsed_var->data.value_of() );
              print_dec(iii, ach, SIGNED);
              }
            SHOW_PARSE_TEXT(ach);
            break;
            }
          }
        }
      SHOW_PARSE_TEXT("<<")
      }
    SHOW_PARSE_END
    }

  // When initializing a variable, we have to ignore any DEPENDING ON clause
  // that might otherwise apply
  suppress_dest_depends = true;

  bool is_redefined = false;

  const cbl_field_t *family_tree = parsed_var;
  while(family_tree)
    {
    if( symbol_redefines(family_tree) )
      {
      is_redefined = true;
      break;
      }

    family_tree = parent_of(family_tree);
    }

  if( parsed_var->level == 66 )
    {
    // Treat RENAMES as if they are redefines:
    is_redefined = true;
    }

  if( parsed_var->data.original() )
    {
    bool a_parent_initialized = false;
    const cbl_field_t *parent = parent_of(parsed_var);
    while( parent )
      {
      if( parent->attr & has_value_e )
        {
        a_parent_initialized = true;
        break;
        }
      parent = parent_of(parent);
      }
    if( !a_parent_initialized )
      {
      parsed_var->attr |= has_value_e;
      }
    }

  int flag_bits  = 0;
  flag_bits     |= explicitly ? EXPLICIT_BIT : 0;
  flag_bits     |= is_redefined && !explicitly ? REDEFINED_BIT : 0 ;
  flag_bits     |=  wsclear()
                    ? DEFAULTBYTE_BIT + (*wsclear() & DEFAULT_BYTE_MASK)
                    : 0;
  flag_bits     |= (refer.nsubscript() << NSUBSCRIPT_SHIFT) & NSUBSCRIPT_MASK;
  flag_bits     |= just_once ? JUST_ONCE_BIT : 0 ;

  suppress_dest_depends = false;  // Set this to false so that refer_is_clean is valid

  if( !refer_is_clean(refer) )
    {
    gg_call(VOID,
            "__gg__initialize_variable",
            gg_get_address_of(refer.field->var_decl_node),
            refer_offset(refer),
            build_int_cst_type(INT, flag_bits),
            NULL_TREE);
    }
  else
    {
    // We have a clean refer with no mods, so we can send just the pointer to
    // the field
    gg_call(VOID,
            "__gg__initialize_variable_clean",
            gg_get_address_of(refer.field->var_decl_node),
            build_int_cst_type(INT, flag_bits) ,
            NULL_TREE);
    }

  suppress_dest_depends = true;

  TRACE1
    {
    TRACE1_HEADER
    if( refer.field->level )
      {
      gg_fprintf( trace_handle,
                  1, "%2.2d ",
                  build_int_cst_type(INT, refer.field->level));
      }
    TRACE1_REFER_INFO("", refer)
    if( refer.field->level == 88 )
      {
      TRACE1_TEXT(" [");

      size_t returned_size = 0;
      char *string88 = get_level_88_domain(0, parsed_var, returned_size);

      char *p = string88;
      bool first = true;
      while(*p)
        {
        char *pend;
        size_t length1 = strtoull(p, &pend, 10);
        char *string1  = pend + 1;
        char flag = *pend;
        p = string1 + length1;
        if( flag == 'A' )
          {
          char ach2[] = "x";
          TRACE1_TEXT("\"")
          for(size_t i=0; i<length1; i++)
            {
            ach2[0] = string1[i];
            TRACE1_TEXT(ach2)
            }
          TRACE1_TEXT("\"")
          }
        else
          {
          switch(string1[0])
            {
            case 'L':
              TRACE1_TEXT("LOW-VALUE")
              break;
            case 'Z':
              TRACE1_TEXT("ZERO")
              break;
            case 'S':
              TRACE1_TEXT("SPACE")
              break;
            case 'Q':
              TRACE1_TEXT("QUOTE")
              break;
            case 'H':
              TRACE1_TEXT("HIGH-VALUE")
              break;
            default:
              TRACE1_TEXT("???")
              break;
            }
          }
        if( first )
          {
          TRACE1_TEXT("/")
          }
        else
          {
          if(*p)
            {
            TRACE1_TEXT(" ")
            }
          }
        first = !first;
        }
      free(string88);
      TRACE1_TEXT("] ");
      }
    else if( parsed_var->type == FldClass )
      {
      char *p = get_class_condition_string(parsed_var);
      TRACE1_TEXT(p);
      free(p);
      }
    else
      {
      // Convert strings of spaces to "<SPACES>"
      tree spaces = gg_define_variable(INT, 0L);
      if(   parsed_var->type == FldGroup
         || parsed_var->type == FldAlphanumeric
         || parsed_var->type == FldAlphaEdited
         || parsed_var->type == FldLiteralA )
        {
        gg_assign(spaces, integer_one_node);
        tree counter = gg_define_variable(INT, parsed_var->data.capacity());
        WHILE(counter, gt_op, integer_zero_node)
          {
          gg_decrement(counter);
          IF( gg_indirect(member(parsed_var->var_decl_node, "data"), counter),
              ne_op,
              build_int_cst_type(UCHAR, ' ') )
              {
              gg_assign(spaces, integer_zero_node);
              }
          ELSE
            {
            }
          ENDIF
          }
          WEND
        }
      IF(spaces, eq_op, integer_one_node)
        {
        TRACE1_TEXT(" <SPACES>")
        }
      ELSE
        {
        TRACE1_FIELD_VALUE("", parsed_var, "")
        }
      ENDIF
      }
    TRACE1_END
    }
  suppress_dest_depends = false;
  }

void
parser_initialize(const cbl_refer_t& refer, bool like_parser_symbol_add)
  {
  //gg_printf("parser_initialize %s\n", gg_string_literal(refer.field->name), NULL_TREE);
  if( like_parser_symbol_add )
    {
    initialize_variable_internal(refer);
    }
  else
    {
    gcc_assert(refer.field->data.original());
    static const bool explicitly = true;
    initialize_variable_internal(refer, explicitly);
    }
  }

static
void
depending_on_value(tree depending_on, cbl_field_t *current_sizer)
  {
  // We have to deal with the possibility of a DEPENDING_ON variable,
  // and we have to apply array bounds whether or not there is a DEPENDING_ON
  // variable:

//  tree occurs_lower = gg_define_variable(LONG, "_lower");
//  tree occurs_upper = gg_define_variable(LONG, "_upper");
//
//  gg_assign(occurs_lower, build_int_cst_type(LONG, current_sizer->occurs.bounds.lower));
//  gg_assign(occurs_upper, build_int_cst_type(LONG, current_sizer->occurs.bounds.upper));

  gcc_assert(current_sizer);
  if( current_sizer->occurs.depending_on )
    {
    get_depending_on_value_from_odo(depending_on, current_sizer);
    }
  else
    {
    gg_assign(depending_on,
              build_int_cst_type(LONG, current_sizer->occurs.bounds.upper));
    }
  }

static tree
tree_type_from_field_type(cbl_field_t *field, size_t &nbytes)
  {
  /*  This routine is used to determine what action is taken with type of a
      CALL ... USING <var> and the matching PROCEDURE DIVISION USING <var> of
      a PROGRAM-ID or FUNCTION-ID
      */
  tree retval = COBOL_FUNCTION_RETURN_TYPE;
  nbytes = 8;
  if( field )
    {
    // This maps a Fldxxx to a C-style variable type:
    switch(field->type)
      {
      case FldGroup:
      case FldAlphanumeric:
      case FldAlphaEdited:
      case FldNumericEdited:
        retval = CHAR_P;
        nbytes = field->data.capacity();
        break;

      case FldNumericDisplay:
      case FldNumericBinary:
      case FldPacked:
      if( field->data.digits > 18 )
          {
          retval = UINT128;
          nbytes = 16;
          }
        else
          {
          retval = SIZE_T;
          nbytes = 8;
          }
        break;

      case FldNumericBin5:
      case FldIndex:
      case FldPointer:
        if( field->data.capacity() > 8 )
          {
          retval = UINT128;
          nbytes = 16;
          }
        else
          {
          retval = SIZE_T;
          nbytes = 8;
          }
        break;

      case FldFloat:
        if( field->data.capacity() == 8 )
          {
          retval = DOUBLE;
          nbytes = 8;
          }
        else if( field->data.capacity() == 4 )
          {
          retval = FLOAT;
          nbytes = 4;
          }
        else
          {
          retval = FLOAT128;
          nbytes = 16;
          }
        break;

      case FldLiteralN:
        // Assume a 64-bit signed integer.  This happens for GOBACK STATUS 101,
        // the like
        retval = LONG;
        nbytes = 8;
        break;

      default:
        cbl_internal_error(  "%s: Invalid field type %s:",
                __func__,
                cbl_field_type_str(field->type));
        break;
      }
    if( retval == SIZE_T && field->attr & signable_e )
      {
      retval = SSIZE_T;
      }
    if( retval == UINT128 && field->attr & signable_e )
      {
      retval = INT128;
      }
    }
  return retval;
  }

static char *
combined_name(const cbl_label_t *label)
  {
  // This routine returns a pointer to a static, so make sure you use the result
  // before calling the routine again
  const char *para_name     = nullptr;
  const char *sect_name     = nullptr;
  const char *program_name  = current_function->our_unmangled_name;

  if( label->type == LblParagraph )
    {
    para_name = label->name;

    if( label->parent )
      {
      // It's possible for implicit
      const cbl_label_t *section_label = cbl_label_of(symbol_at(label->parent));
      sect_name = section_label->name;
      }
    }
  else
    {
    sect_name = label->name;
    }

  static size_t retval_size = 256;
  static char *retval= static_cast<char *>(xmalloc(retval_size));

  char *paragraph             = cobol_name_mangler(para_name);
  char *section         = cobol_name_mangler(sect_name);
  char *mangled_program_name  = cobol_name_mangler(program_name);

  while( retval_size < (paragraph ? strlen(paragraph) : 0 )
                  + (section ? strlen(section) : 0 )
                  + (mangled_program_name ? strlen(mangled_program_name) : 0 )
                  + 24 )
    {
    retval_size *= 2;
    retval = static_cast<char *>(xrealloc(retval, retval_size));
    }
  gcc_assert(retval);

  *retval = '\0';
  char ach[24];
  if( paragraph )
    {
    strcat(retval, paragraph);
    }
  strcat(retval, ".");
  if( section )
    {
    strcat(retval, section);
    }
  strcat(retval, ".");
  if( mangled_program_name )
    {
    strcat(retval, mangled_program_name);
    }
  sprintf(ach, "." HOST_SIZE_T_PRINT_DEC,
          (fmt_size_t)current_function->program_id_number);
  strcat(retval, ach);
  sprintf(ach, "." HOST_SIZE_T_PRINT_DEC,
          (fmt_size_t)symbol_label_id(label));
  strcat(retval, ach);
  free(mangled_program_name);
  free(section);
  free(paragraph);

  return retval;
  }

// We implement SECTION and PARAGRAPH stuff before the rest of program
// structure, because we have some static routines in here that are called
// by enter_ and leave_ program, and so on.

static void
assembler_label(const char *label)
  {
  // label has to be a valid label for the assembler
  static size_t length = 0;
  static char *build = nullptr;

  const char local_text[] = ":";
  if( length < strlen(label) + strlen(local_text) + 1 )
    {
    length = strlen(label) + strlen(local_text) + 1;
    free(build);
    build = static_cast<char *>(xmalloc(length));
    }
  gcc_assert(build);

  strcpy(build, label);
  strcat(build, local_text);

  gg_insert_into_assembler(build);
  }

static void
section_label(struct cbl_proc_t *procedure)
  {
  // With nested programs, you can have multiple program/section pairs with the
  // the same names; we use a deconflictor to avoid collisions

  size_t deconflictor = symbol_label_id(procedure->label);

  cbl_label_t *label = procedure->label;
  // The _initialize_program section isn't relevant.
  char *psz = xasprintf("%s SECTION %s in %s (" HOST_SIZE_T_PRINT_DEC ")",
                        ASM_COMMENT_START,
                        label->name,
                        current_function->our_unmangled_name,
                        (fmt_size_t)deconflictor);
  gg_insert_into_assembler(psz);
  free(psz);

  // The label has to start with an underscore.  I tried a period, but those
  // don't seem to show up in GDB's internal symbol tables.
  char *psz2 = xasprintf( "_sect.%s",
                          combined_name(procedure->label));
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(psz2);
    SHOW_PARSE_END
    }
  assembler_label(psz2);
  free(psz2);
  // Needed so that GDB-COBOL can trap at a section name.
  insert_nop(101);

  // Go see if there was an ALTER statement targeting this procedure
  gg_append_statement(procedure->alter_switch_goto);
  // Lay down the label we will return to if there is no ALTER in play
#if 0
  fprintf(stderr,
          "section_label for %s %s\n",
          procedure->label->name,
          label_decl_text_from_expr(procedure->no_alter_label));
#endif
  gg_append_statement(procedure->no_alter_label);
  }

static void
paragraph_label(struct cbl_proc_t *procedure)
  {
  // We need to give each paragraph a unique and assembler-compatible name
  // that can be found and used by GDB.
  // Complications:
  //   1) paragraph names can be reused in the same program, provided they
  //      are in different sections.
  //   2) paragraph names can be duplicated in a section, provided that they
  //      are not referenced by the program.  We provide a deconflictor to
  //      separate such labels.

  cbl_label_t *paragraph  = procedure->label;
  cbl_label_t *section    = nullptr;

  if( procedure->label->parent )
    {
    section = cbl_label_of(symbol_at(procedure->label->parent));
    }

  char *para_name    = paragraph->name;
  char *section_name = section ? section->name : nullptr;

  size_t deconflictor = symbol_label_id(procedure->label);

  char *psz1 =
  xasprintf(
          "%s PARAGRAPH %s of %s in %s (" HOST_SIZE_T_PRINT_DEC ")",
          ASM_COMMENT_START,
          para_name ? para_name: "" ,
          section_name ? section_name: "(null)" ,
          current_function->our_unmangled_name ? current_function->our_unmangled_name: "" ,
          (fmt_size_t)deconflictor );

  // (0) is wrong, so back up one

  gg_insert_into_assembler(psz1);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(psz1);
    SHOW_PARSE_END
    }
  free(psz1);

  // The label has to start with an underscore.  I tried a period, but those
  // don't seem to show up in GDB's internal symbol tables.
  char *psz2 = xasprintf( "_para.%s",
                          combined_name(procedure->label));
  assembler_label(psz2);
  free(psz2);

  // We are inserting a NOP after having created a label for the procedure.
  // This means that when using GDC_COBOL to step into a procedure, the
  // execution will stop there and show "123 para-name." at the stopped point.
  //
  // Note that because there is no user-specified executable code at that point
  // the user can't set a working breakpoint with "break 123".  But because
  // GDB will pick up the psz2 text and set a breakpoint there (which is the
  // location of the NOP) "break para-name" will actually stop and show line
  // 123.
  //
  // This really only makes sense when you look at the assembly language. Keep
  // in mind as you read it that issuing a "break 123" causes GDB to set a
  // breakpoint at the first executable machine language code following the
  // first ".loc 123" directive.
  //
  // Yes, trying to understand this causes headaches for many people who read
  // this.  Take an aspirin.
  insert_nop(102);

  // Go see if there was an ALTER statement targeting this procedure
  gg_append_statement(procedure->alter_switch_goto);
  // Lay down the label we will return to if there is no ALTER in play
#if 0
  fprintf(stderr,
          "paragraph_label for %s %s\n",
          procedure->label->name,
          label_decl_text_from_expr(procedure->no_alter_label));
#endif
  gg_append_statement(procedure->no_alter_label);
  }

static void
pseudo_return_push(cbl_proc_t *procedure, size_t index)
  {
  // Put the return address onto the stack:
  //gg_suppress_location(true);

  TRACE1
    {
    TRACE1_HEADER
    gg_printf("%s %p %ld",
              gg_string_literal(procedure->label->name),
              gg_cast(SIZE_T, procedure->exit.addr),
              build_int_cst_type(SIZE_T, index),
              NULL_TREE);
    TRACE1_END
    }

  gg_call(VOID,
          "__gg__pseudo_return_push",
          procedure->exit.addr,
          build_int_cst_type(SIZE_T, index),
          NULL_TREE);
  }

static void
pseudo_return_pop(cbl_proc_t *procedure)
  {
  TRACE1
    {
    TRACE1_HEADER
    gg_printf("%s comparing proc_exit %p to global_exit %p -- ",
              gg_string_literal(procedure->label->name),
              gg_cast(SIZE_T, procedure->exit.addr),
              var_decl_exit_address,
              NULL_TREE);
    }

  token_location_override(current_location_minus_one());
  IF( var_decl_exit_address, eq_op, procedure->exit.addr )
    {
    TRACE1
      {
      TRACE1_TEXT("Returning")
      TRACE1_END
      }
    // The top of the stack is us!

    // Pick up the return index from the pseudo_return stack:
    token_location_override(current_location_minus_one());

    // And do the return:
    token_location_override(current_location_minus_one());
    gg_append_statement(procedure->dispatch_switch_goto);
    }
  ELSE
    {
    TRACE1
      {
      TRACE1_TEXT("No match")
      }
    ENDIF
    }
  TRACE1
    {
    TRACE1_END
    }
  }

static void
leave_procedure(struct cbl_proc_t *procedure, bool /*section*/)
  {
  if(procedure)
    {
    // fprintf(stderr, "LeavingProcedure: (%p) %s %p %p %p %p %p %p\n",
    // procedure,
    // procedure->name,
    // procedure->top.go_to,
    // procedure->top.label,
    // procedure->exit.go_to,
    // procedure->exit.label,
    // procedure->bottom.go_to,
    // procedure->bottom.label);
    // Procedure can be null, for example at the beginning of a
    // new program, or after somebody else has cleared it out.
    gg_append_statement(procedure->exit.label);
    pseudo_return_pop(procedure);
    gg_append_statement(procedure->bottom.label);
    }
  }

static void
leave_section_internal()
  {
  Analyze();
  SHOW_PARSE
    {
    if(   gg_trans_unit.function_stack.size()
       && current_function && current_function->current_section)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(current_function->current_section->label->name)
      SHOW_PARSE_TEXT(" ")
      fprintf(stderr,
              "%p",
              static_cast<void *>(current_function->current_section->label));
      SHOW_PARSE_END
      }
    }

  if( current_function->current_section )
    {
    // gg_printf(  "Leaving section %s\n",
    // build_string_literal( strlen(current_function->current_section->label->name)+1, current_function->current_section->label->name),
    // NULL_TREE);
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT_ABC("\"", current_function->current_section->label->name, "\"");
      TRACE1_END
      }
    leave_procedure(current_function->current_section, true);

    current_function->current_section = NULL;
    }
  else
    {
    //gg_printf("Somebody is leaving a section twice\n", NULL_TREE);
    }
  }

void
parser_leave_section( struct cbl_label_t */*label*/ ) {}

static void
leave_paragraph_impl()
  {
  Analyze();
  SHOW_PARSE
    {
    if(gg_trans_unit.function_stack.size() && current_function && current_function->current_paragraph)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(current_function->current_paragraph->label->name)
      SHOW_PARSE_END
      }
    }

  if( current_function->current_paragraph )
    {
    // gg_printf(  "Leaving paragraph %s\n",
    // build_string_literal( strlen(current_function->current_paragraph->label->name)+1, current_function->current_paragraph->label->name),
    // NULL_TREE);
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT_ABC("\"", current_function->current_paragraph->label->name, "\"");
      TRACE1_END
      }
    leave_procedure(current_function->current_paragraph, false);
    current_function->current_paragraph = NULL;
    }
  else
    {
    //gg_printf("Somebody is leaving a paragraph twice\n", NULL_TREE);
    }
  }

void parser_leave_paragraph( cbl_label_t * ) {}
static inline void leave_paragraph_internal() { leave_paragraph_impl(); }

static struct cbl_proc_t *
find_procedure(cbl_label_t *label)
  {
//    SHOW_PARSE
//        {
//        SHOW_PARSE_HEADER
//        SHOW_PARSE_LABEL(" ", label)
//        SHOW_PARSE_TEXT("\n");
//        }

  cbl_proc_t *retval  = label->structs.proc;

  //  We have to cope with an oddball circumstance.  When label->entered is
  //  greater than zero, it means that a paragraph with this label has been
  //  entered and left already.  This means that a paragraph name has been
  //  defined more than once.  Had it been referenced with a GOTO or PERFORM,
  //  that would have been a syntax error.
  //
  //
  //  In this case, we need to replace the existing cbl_proc_t structure. We
  //  will be laying down labels for this second (or more) instance of
  //  parser_enter_paragraph, and we must create different labels.

  if( !retval )
    {
    // This is a new section or paragraph; we need to create its values:
    //retval = static_cast<struct cbl_proc_t *>(xmalloc(sizeof(struct cbl_proc_t)));
    retval = new struct cbl_proc_t;
    gcc_assert(retval);
    retval->label = label;

    gg_create_goto_pair(&retval->top.go_to,
                        &retval->top.label,
                        &retval->top.addr,
                        &retval->top.decl);
    gg_create_goto_pair(&retval->exit.go_to,
                        &retval->exit.label,
                        &retval->exit.addr);
    gg_create_goto_pair(&retval->bottom.go_to,
                        &retval->bottom.label,
                        &retval->bottom.addr);

    // We need a goto/label pair for the location of the dispatch switch for
    // this paragraph:
    gg_create_goto_pair(&retval->dispatch_switch_goto,
                        &retval->dispatch_switch_label);

    // We need goto/label pairs for the location of the dispatch switch for
    // any potential ALTER to this paragraph
    gg_create_goto_pair(&retval->alter_switch_goto,
                        &retval->alter_switch_label);
    gg_create_goto_pair(&retval->no_alter_goto,
                        &retval->no_alter_label);

    // We can now add this procedure to the of paragraphs that might be
    // performed:
    current_function->list_of_procedures.push_back(retval);

    // When this paragraph becomes the target of an ALTER statement, the index
    // that will be used in the switch() statement goes here:
    retval->alter_index = gg_define_variable(SIZE_T, NULL, vs_static, 0);

    label->structs.proc = retval;
    }

  return retval;
  }

void
parser_enter_section(cbl_label_t *label)
  {
  Analyze();

  RETURN_WHEN_HIJACKED;

  // Do the leaving before the SHOW_PARSE; it makes the output more sensible
  // A new section ends the current paragraph:
  leave_paragraph_internal();

  // And the current section:
  leave_section_internal();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    SHOW_PARSE_INDENT
    linemap_dump_location( line_table, current_token_location(), stderr );
    SHOW_PARSE_END
    }

  CHECK_LABEL(label);

  // This NOP is needed to give GDB a line number for the entry point of
  // paragraphs
  insert_nop(103);

  struct cbl_proc_t *procedure = find_procedure(label);
  gg_append_statement(procedure->top.label);
  section_label(procedure);
  current_function->current_section = procedure;

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("\"", label, "\"")
    TRACE1_END
    }
  }

void
parser_enter_paragraph(cbl_label_t *label)
  {
  Analyze();

  RETURN_WHEN_HIJACKED;

  // Do the leaving before the SHOW_PARSE; the output makes more sense that way
  // A new paragraph ends the current paragraph:
  leave_paragraph_internal();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    SHOW_PARSE_INDENT
    linemap_dump_location( line_table, current_token_location(), stderr );
    SHOW_PARSE_END
    }

  CHECK_LABEL(label);

  struct cbl_proc_t *procedure = find_procedure(label);

  gg_append_statement(procedure->top.label);
  paragraph_label(procedure);
  current_function->current_paragraph = procedure;

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("\"", label, "\"")
    TRACE1_END
    }
  }

void
parser_exit_section(void)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("\"", current_function->current_section->label->name, "\"")
    TRACE1_END
    }
  gg_append_statement(current_function->current_section->exit.go_to);
  }

void
parser_exit_paragraph(void)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("\"", current_function->current_paragraph->label->name, "\"")
    TRACE1_END
    }
  gg_append_statement(current_function->current_paragraph->exit.go_to);
  }

void
parser_exit_perform(struct cbl_perform_tgt_t *tgt, bool cycle)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  if(cycle)
    {
    gg_append_statement(tgt->addresses.testA.go_to);
    }
  else
    {
    gg_append_statement(tgt->addresses.exit.go_to);
    }
  }

void
parser_alter( cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  cbl_label_t *altered    = tgt->from();
  cbl_label_t *proceed_to = tgt->to();

  struct cbl_proc_t *altered_proc = find_procedure(altered);
  struct cbl_proc_t *proceed_to_proc = find_procedure(proceed_to);

  // We add one to the size of the alter_decls list, because we use zero to
  // indicate that alter_index hasn't been changed.
  gg_assign(altered_proc->alter_index,
            build_int_cst_type(SIZE_T,
                               altered_proc->alter_decls.size()+1));
  altered_proc->alter_decls.push_back(proceed_to_proc->top.addr);
  }

void
parser_goto(const cbl_refer_t &value_ref,
            size_t narg,
            cbl_label_t * const labels[] )
  {
  // This is part of the Terrible Trio of parser_perform, parser_goto and
  // parser_enter_[procedure].  parser_goto has an easier time of it than
  // the other two, because it just has to jump from here to the entry point
  // of the paragraph [or section]
  Analyze();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    for(size_t i=0; i<narg; i++)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(labels[i]->name);
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    for(size_t i=0; i<narg; i++)
      {
      TRACE1_TEXT(labels[i]->name);
      TRACE1_TEXT(" ");
      }
    TRACE1_END
    }

  gcc_assert(narg >= 1);

  if( narg == 1 )
    {
    // This is the simplest possible case -- no DEPENDING ON clause.
    struct cbl_proc_t *procedure = find_procedure(labels[0]);
    gg_append_statement(procedure->top.go_to);
    }
  else
    {
    // We will implement the two or more fanout with a switch statement.

    tree value;
    get_binary_value(value, value_ref, INT);

    // value is properly 1 through nargs

    tree switch_statement_list = make_node(STATEMENT_LIST);
    TREE_TYPE(switch_statement_list) = void_type_node;

    tree switchexpr = build2(SWITCH_EXPR,
                             integer_type_node,
                             value,
                             switch_statement_list);
    gg_append_statement(switchexpr);
    current_function->statement_list_stack.push_back(switch_statement_list);

    tree caselabel;
    tree labeldecl;

    for(size_t i = 0; i < narg; ++i)
      {
      tree val = build_int_cst(INT, i+1);
      labeldecl = create_artificial_label(UNKNOWN_LOCATION);
      DECL_CONTEXT(labeldecl) = current_function->function_decl;
      caselabel = build_case_label(val,
                                   NULL_TREE,
                                   labeldecl);
      gg_append_statement(caselabel);

      struct cbl_proc_t *procedure = find_procedure(labels[i]);
      gg_append_statement(procedure->top.go_to);
      }

    // Finish with a default case that just falls through
    labeldecl = create_artificial_label(UNKNOWN_LOCATION);
    DECL_CONTEXT(labeldecl) = current_function->function_decl;

    caselabel = build_case_label(NULL_TREE,
                                 NULL_TREE,
                                 labeldecl);
    gg_append_statement(caselabel);

    current_function->statement_list_stack.pop_back();
    }
  }

void
parser_perform_times( cbl_label_t *proc_1, const cbl_refer_t &count )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", proc_1)
    SHOW_PARSE_REF(" ", count)
    SHOW_PARSE_TEXT(" TIMES")
    char ach[32];
    sprintf(ach, " proc_1 is at %p", static_cast<void*>(proc_1));
    SHOW_PARSE_TEXT(ach)
    sprintf(ach, " proc_1->proc is %p",
            static_cast<void*>(proc_1->structs.proc));
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  perform_is_armed = CURRENT_LINE_NUMBER ;

  // Get the count:
  tree counter;
  get_binary_value(counter, count, LONG);

  // Make sure the initial count is valid:
  WHILE( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    {
    static const bool suppress_nexting = true;
    parser_perform(proc_1, suppress_nexting);
    gg_decrement(counter);
    }
    WEND
  }

static void
internal_perform_through( cbl_label_t *proc_1,
                          cbl_label_t *proc_2,
                          bool suppress_nexting )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", proc_1);
    char ach[32];
    sprintf(ach, " proc_1 is at %p", static_cast<void*>(proc_1));
    SHOW_PARSE_TEXT(ach)
    if( proc_1 )
      {
      sprintf(ach,
              " proc_1->proc is %p",
              static_cast<void*>(proc_1->structs.proc));
      }
    SHOW_PARSE_TEXT(ach)
    if( proc_2 )
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_LABEL_OK("", proc_2);
      sprintf(ach, " proc_2 is at %p", static_cast<void*>(proc_2));
      SHOW_PARSE_TEXT(ach)
      sprintf(ach, " proc_2->proc is %p", static_cast<void*>(proc_2->structs.proc));
      SHOW_PARSE_TEXT(ach)
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  CHECK_LABEL(proc_1);

  if( !proc_2 )
    {
    proc_2 = proc_1;
    }

  struct cbl_proc_t *proc1 = find_procedure(proc_1);
  struct cbl_proc_t *proc2 = find_procedure(proc_2);

  size_t dispatch_index = proc2->pseudo_return_decls.size();

  // We need to create the return address that we
  // will instantiate right after the goto:

  static int id = 1;
  char *psz;
  psz = xasprintf("_perfret%d", id++);

  tree return_address_decl = build_decl(  UNKNOWN_LOCATION,
                                          LABEL_DECL,
                                          gg_create_assembler_name(psz),
                                          void_type_node);
  DECL_CONTEXT(return_address_decl) = current_function->function_decl;
  TREE_USED(return_address_decl) = 1;
  free(psz);

  tree return_label_expr = build1(LABEL_EXPR,
                                  void_type_node,
                                  return_address_decl);

  // Put the dispatch_index for this PERFORM onto the stack
  pseudo_return_push(proc2, dispatch_index);

  // Create the code that will launch the first procedure
  if( proc_1 != proc_2 )
    {
    gg_insert_into_assemblerf("%s PERFORM %s THROUGH %s",
                          ASM_COMMENT_START, proc_1->name, proc_2->name);
    }
  else
    {
    gg_insert_into_assemblerf("%s PERFORM %s",
                          ASM_COMMENT_START, proc_1->name);
    }

  if( !suppress_nexting )
    {
    perform_is_armed = CURRENT_LINE_NUMBER ;
    }

  gg_append_statement(proc1->top.go_to);

  // And create the return address label:
  gg_append_statement(return_label_expr);

  // Now we add the return location for the PERFORM to the vector of such
  // locations for proc2:
  proc2->pseudo_return_decls.push_back(return_address_decl);
  }

void
parser_perform(cbl_label_t *label, bool suppress_nexting)
  {
  return internal_perform_through(label, NULL, suppress_nexting);
  }

static void
internal_perform_through_times(   cbl_label_t *proc_1,
                                  cbl_label_t *proc_2,
                            const cbl_refer_t &count)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", proc_1);
    char ach[32];
    sprintf(ach, " proc_1 is at %p", static_cast<void*>(proc_1));
    SHOW_PARSE_TEXT(ach)
    if( proc_1 )
      {
      sprintf(ach,
              " proc_1->proc is %p",
              static_cast<void*>(proc_1->structs.proc));
      }
    SHOW_PARSE_TEXT(ach)
    if( proc_2 )
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_LABEL_OK("", proc_2);
      sprintf(ach, " proc_2 is at %p", static_cast<void*>(proc_2));
      SHOW_PARSE_TEXT(ach)
      sprintf(ach, " proc_2->proc is %p", static_cast<void*>(proc_2->structs.proc));
      SHOW_PARSE_TEXT(ach)
      }
    SHOW_PARSE_REF(" ", count);
    SHOW_PARSE_TEXT(" TIMES");
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  perform_is_armed = CURRENT_LINE_NUMBER ;

  tree counter;
  get_binary_value(counter, count, LONG);
  WHILE( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    {
    internal_perform_through(proc_1, proc_2, true); // true means suppress_nexting
    gg_decrement(counter);
    }
    WEND
  }

void
register_main_switch(const char *main_string)
  {
  char *mstr = xstrdup(main_string);
  char *p = strchr(mstr, ':');
  if( p )
    {
    *p = '\0';
    main_string = p+1;
    main_strings[mstr] = main_string;
    }
  else
    {
    main_strings[mstr] = "";
    }
  free(mstr);
  }

static int file_level = 0;

void
parser_first_statement( int lineno )
  {
  // In the event that this routine is the one that main() calls to get the
  // execution ball rolling, we want the GDB "start" function to be able
  // to set a temporary breakpoint at this location.  We get that rolling
  // here.

  char ach[256];

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    sprintf(ach, " lineno is %d, suppression is %d", lineno, suppress_cobol_entry_point);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  if(    strcmp(current_function->our_name, ach_cobol_entry_point) == 0
      && !suppress_cobol_entry_point )
    {
    sprintf(ach,
            "%s:%d",
            current_filename.back().c_str(),
            lineno);
    *ach_cobol_entry_point = '\0';
    create_cblc_string_variable("_cobol_entry_point", ach);

    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach2[512];
      sprintf(ach2, "setting _cobol_entry_point to \"%s\"", ach);
      SHOW_PARSE_TEXT(ach2)
      SHOW_PARSE_END
      }
    }

  if( !suppress_cobol_entry_point )
    {
    char achentry[128];
    sprintf(ach,
            "%s:%d",
            current_filename.back().c_str(),
            lineno);

    sprintf(achentry, "_prog_entry_point_%s", current_function->our_name);
    create_cblc_string_variable(achentry, ach);
    }
  }

void
parser_enter_file(const char *filename)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char *psz;
    psz = xasprintf(" entering level:%d %s", file_level+1, filename);
    SHOW_PARSE_TEXT(psz);
    free(psz);
    SHOW_PARSE_END
    }

  current_filename.push_back(filename);

  std::unordered_map<std::string, std::string>::const_iterator it
    = main_strings.find(filename);

  if( it != main_strings.end() )
    {
    // There was a -main switch for this file.
    this_module_has_main = true;
    next_program_is_main = true;

    const char *pname = it->second.c_str();
    if( pname && strlen(pname) )
      {
      main_entry_point = xstrdup(pname);
      }
    }

  if( file_level == 0 )
    {
    // Build a translation_unit_decl:
    gg_build_translation_unit(filename);
    create_our_type_nodes();
    }

  file_level += 1;

  if( file_level == 1 )
    {
    // This table is used for "creating" the file-static named variables used in
    // the GENERIC we generate.

  // Establish our variable declarations for global variables in libgcobol:

#define SET_VAR_DECL(A, B, C) \
  A = gg_declare_variable(B, C, NULL_TREE, vs_extern)

    SET_VAR_DECL(var_decl_exception_code         , INT    , "__gg__exception_code");
    SET_VAR_DECL(var_decl_exception_file_status  , INT    , "__gg__exception_file_status");
    SET_VAR_DECL(var_decl_exception_file_name    , CHAR_P , "__gg__exception_file_name");
    SET_VAR_DECL(var_decl_exception_statement    , CHAR_P , "__gg__exception_statement");
    SET_VAR_DECL(var_decl_exception_source_file  , CHAR_P , "__gg__exception_source_file");
    SET_VAR_DECL(var_decl_exception_line_number  , INT    , "__gg__exception_line_number");
    SET_VAR_DECL(var_decl_exception_program_id   , CHAR_P , "__gg__exception_program_id");
    SET_VAR_DECL(var_decl_exception_section      , CHAR_P , "__gg__exception_section");
    SET_VAR_DECL(var_decl_exception_paragraph    , CHAR_P , "__gg__exception_paragraph");

    SET_VAR_DECL(var_decl_default_compute_error  , INT    , "__gg__default_compute_error");
    SET_VAR_DECL(var_decl_rdigits                , INT    , "__gg__rdigits");
    SET_VAR_DECL(var_decl_unique_prog_id         , SIZE_T , "__gg__unique_prog_id");

    SET_VAR_DECL(var_decl_exit_address           , VOID_P , "__gg__exit_address");

    SET_VAR_DECL(var_decl_call_parameter_signature , CHAR_P   , "__gg__call_parameter_signature");
    SET_VAR_DECL(var_decl_call_parameter_count     , INT      , "__gg__call_parameter_count");
    SET_VAR_DECL(var_decl_call_parameter_lengths   , build_array_type(SIZE_T, NULL),
                                                            "__gg__call_parameter_lengths");

    SET_VAR_DECL(var_decl_nop                     , INT                     , "__gg__nop"             );
    SET_VAR_DECL(var_decl_main_called             , INT                     , "__gg__main_called"     );
    SET_VAR_DECL(var_decl_entry_index             , SIZE_T                  , "__gg__entry_index"     );
    SET_VAR_DECL(var_decl_dialects                , INT                     , "__gg__dialects"        );
    SET_VAR_DECL(var_decl_dp2bin                  , build_array_type(UCHAR, NULL), "__gg__dp2bin");
    }
  }

void
parser_leave_file()
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[256];
    sprintf(ach,
            "leaving level:%d %s",
            file_level,
            current_filename.back().c_str());
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }
  file_level -= 1;
  current_filename.pop_back();

  if( file_level == 0 )
    {
    // We are leaving the top-level file, which means this compilation is
    // done, done, done.

    if( !hijacked )
      {
      // This is where we create the file-static table of PERFORM/FOLLOWING line
      // number pairs so that the GDB-COBOL debugger can know where to "return"
      // to after a NEXT is issued on a PERFORM statement.

      // We need to create a file-static static array of 32-bit integers.  The
      // array is terminated with a {0,0} pair:
      tree array_of_int_type = build_array_type_nelts(INT, (perform_line_pairs.size()+1)*2);
      tree array_of_int = gg_define_variable( array_of_int_type,
                                              "_perform_line_pairs",
                                              vs_file_static);
      // We have the array.  Now we need to build the constructor for it
      tree constr = make_node(CONSTRUCTOR);
      TREE_TYPE(constr) = array_of_int_type;
      TREE_STATIC(constr)    = 1;
      TREE_CONSTANT(constr)  = 1;

      // The first element of the array contains the number of elements to follow
      size_t i = 0;
      for(auto it : perform_line_pairs)
        {
        CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                                build_int_cst_type(SIZE_T, i++),
                                build_int_cst_type(INT, it.first) );
        CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                                build_int_cst_type(SIZE_T, i++),
                                build_int_cst_type(INT, it.second) );
        }
      CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                              build_int_cst_type(SIZE_T, i++),
                              integer_zero_node );
      CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                              build_int_cst_type(SIZE_T, i++),
                              integer_zero_node );
      DECL_INITIAL(array_of_int) = constr;

      // There is, however, one thing left to do.  If the command line says
      // that this module needs a main entry point, then this is where
      // we create a main() function.  We build it at the end, so that all of
      // the .loc directives associated with it appear at the end of the
      // source code.  We used to create the main() entry point at the beginning,
      // but that created confusion for GDB when trying to debug the generated
      // executable.
      if( main_entry_point )
        {
        next_program_is_main = false;
        build_main_that_calls_something(main_entry_point);
        free(main_entry_point);
        main_entry_point = NULL;
        }
      }

    gg_leaving_the_source_code_file();
    }
  }

void
enter_program_common(const char *funcname, const char *funcname_)
  {
  // We arrive here when processing a PROGRAM-ID.

  // At this point, we don't know how many formal parameters there are going
  // to be.

  // We are going to create a function returning a 64-bit value, but it'll
  // have no parameters.  We'll chain the parameters on in parser_division(),
  // when we process PROCEDURE DIVISION USING...

  gg_define_function(COBOL_FUNCTION_RETURN_TYPE,
                     funcname,
                     funcname_,
                     NULL_TREE);

  current_function->first_time_through =
                                  gg_define_variable(INT,
                                                      "_first_time_through",
                                                      vs_static,
                                                      integer_one_node);

  gg_create_goto_pair(&current_function->skip_init_goto,
                      &current_function->skip_init_label);

  IF( current_function->first_time_through, eq_op, integer_zero_node )
    gg_append_statement(current_function->skip_init_goto);
  ELSE
    ENDIF

  gg_assign(current_function->first_time_through, integer_zero_node);

  current_function->perform_exit_address =
                         gg_define_variable (VOID_P, "_perform_exit_address");

  // Make sure the following are null, because when we create the unnamed
  // default section, parser_enter_section will attempt to close them out. And
  // it's possible on the first go-through that they have garbage values.

  current_function->current_section = NULL;
  current_function->current_paragraph = NULL;

  gg_call(VOID,
          "__gg__codeset_figurative_constants",
          NULL_TREE);

  static int counter=1;
  char ach[32];

  sprintf(ach, "_cf_fds_%d", counter);
  current_function->first_declarative_section
                  = gg_define_variable(CHAR_P,
                                       ach,
                                       vs_static,
                                       null_pointer_node);
  sprintf(ach, "_cf_cbmc_%d", counter);
  current_function->called_by_main_counter = gg_define_variable(INT,
                                                          ach,
                                                          vs_static,
                                                          integer_zero_node);
  counter += 1;

  // Initialize the TRACE logic, which has to be done before the first TRACE1
  // invocation, but after there is a function to lay down GIMPLE code in.

  // That is to say: Here.  Multiple invocations of trace1_init are harmless.
  trace1_init();
  }

/*  Creates a function for program-id 'funcname_'.  Returns 1 when funcname_ is
    "main" and the -main compiler switch is active for this module symbol_table
    has been initialized, and the current program has been entered into it. For
    a top-level program, the program's program is 0, else it is the symbol
    table index of the containing program.  */

void
parser_enter_program( const char *funcname_,
                      bool is_function,  // True for user-defined-function
                      int *pretval)
  {
  *pretval = 0;

  // The first thing we have to do is mangle this name.  This is safe even
  // though the end result will be mangled again, because the mangler doesn't
  // change a mangled name.

  char *mangled_name = cobol_name_mangler(funcname_);

  size_t iprog  = current_program_index();
  assert(iprog);

  size_t parent_index = symbol_at(iprog)->program;
  char *funcname;
  if( parent_index )
    {
    // This is a nested function.  Tack on the parent_index to the end of it.
    funcname = xasprintf( "%s." HOST_SIZE_T_PRINT_DEC,
                          mangled_name,
                          (fmt_size_t)parent_index);
    }
  else
    {
    // This is a top-level function; just use the straight mangled name
    funcname = xstrdup(mangled_name);
    }
  free(mangled_name);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(funcname)
    SHOW_PARSE_END
    }

  if( !is_function && !parent_index )
    {
    // This is a top_level program-id, and not a function
    if( next_program_is_main )
      {
      // This is the first top-level program-id.
      next_program_is_main = false;
      if( !main_entry_point )
        {
        // Because no explicit main_entry_point was specified, this program-id,
        // the first in the file, becomes the target of the main() function
        // that will be created at parser_leave_file time.
        main_entry_point = xstrdup(funcname);

        char *psz = cobol_name_mangler(main_entry_point);
        strncpy(ach_cobol_entry_point, psz, sizeof(ach_cobol_entry_point)-1);
        free(psz);
        }
      }
    }

  if( strcmp(funcname_, "main") == 0 && this_module_has_main )
    {
    // Setting 'retval' to 1 lets the caller know that we are being told
    // both to synthesize a main() entry point to duplicate GCC's default
    // behavior, and to create an explicit entry point named "main".  This will
    // eventually result in a link error (because of the duplicated entry
    // points.  The return value serves as an alert; it's up to the caller to
    // decide what to do.
    *pretval = 1;
    }

#ifdef ENABLE_HIJACKING
  if( strcmp(funcname, "dubner_h") == 0)
    {
    fprintf(stderr, "This is a DUBNER hijacking\n");
    hijack_for_development(funcname);
    return;
    }

#endif

  enter_program_common(funcname, funcname_);
  current_function->is_function = is_function;

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("entered program \"")
    TRACE1_TEXT(funcname)
    TRACE1_TEXT("\"")
    TRACE1_END
    }

  free(funcname);
  }

static class label_verify_t {
  std::set<size_t> lain, dangling;
  static inline size_t index_of( const cbl_label_t *label ) {
    return symbol_index(symbol_elem_of(label));
  }
public:
  void go_to( const cbl_label_t *label ) {
    auto p = lain.find(index_of(label));
    if( p == lain.end() ) {
      dangling.insert(index_of(label));
    }
  }
  void lay( const cbl_label_t *label ) {
    auto ok = lain.insert(index_of(label));
    if( ok.second ) {
      dangling.erase(index_of(label));
    }
  }
  bool vet() const { // be always agreeable, for now.
    return dangling.empty();
  }
  void dump() const {
    fprintf(stderr, "%u nonexistent labels called\n", unsigned(dangling.size()) );
    for( auto sym : dangling ) {
      const cbl_label_t *label = cbl_label_of(symbol_at(sym));
      fprintf(stderr, "\t %s\n", label->name);
    }
  }
} label_verify;

static void
build_dispatch_switch(const std::vector<tree> &label_decls)
  {
  // This routine accepts vector of LABEL_DECLs.  It creates a
  // switch statement that's equivalent to
  //      switch(N)
  //         {
  //         default:
  //         case 0:
  //             goto label[0];
  //         case 1:
  //             goto label[1];
  //         ...
  //         case N-1:
  //             goto label[N-1];
  //         }

  // If the vector of label_decls is empty, there is no need to create the
  // switch statement.

  if( !label_decls.empty() )
    {
    tree switch_statement_list = make_node(STATEMENT_LIST);
    TREE_TYPE(switch_statement_list) = void_type_node;

    tree switchexpr = build2(SWITCH_EXPR,
                             integer_type_node,
                             gg_call_expr( SIZE_T,
                                          "__gg__pseudo_return_pop",
                                          NULL_TREE),
                             switch_statement_list);


    gg_append_statement(switchexpr);
    current_function->statement_list_stack.push_back(switch_statement_list);

    // Start off with a "default:" case
    tree labeldecl = create_artificial_label(UNKNOWN_LOCATION);
    DECL_CONTEXT(labeldecl) = current_function->function_decl;
    TREE_USED(labeldecl) = 1;

    tree caselabel;
    caselabel = build_case_label(NULL_TREE,
                                 NULL_TREE,
                                 labeldecl);
    gg_append_statement(caselabel);

    for(size_t i = 0; i < label_decls.size(); ++i)
      {
      // Start with the case label for the pseudo-return location.
      tree val = build_int_cst(SIZE_T, i);

      labeldecl = create_artificial_label(UNKNOWN_LOCATION);
      DECL_CONTEXT(labeldecl) = current_function->function_decl;

      caselabel = build_case_label(val,
                                   NULL_TREE,
                                   labeldecl);
      gg_append_statement(caselabel);

      // And follow up with a goto expression for the pseudo-return location.
      tree goto_expr  = build1( GOTO_EXPR,
                                void_type_node,
                                label_decls[i]);
      gg_append_statement(goto_expr);
      }

    current_function->statement_list_stack.pop_back();
    }
  }

static void
build_alter_switch(cbl_proc_t *proc, const std::vector<tree> &label_decls)
  {
  // This routine accepts a vector of LABEL_DECLs.  It lays down code
  // equivalent to
  //    if( label_decls.size() )
  //      {
  //      switch(N)
  //         {
  //         case 0:
  //             goto proc->no_alter_label;
  //         case 1:
  //             goto label[0];
  //         ...
  //         case N:
  //             goto label[N-1];
  //         default:
  //         }
  //       }
  //     goto proc->no_alter_label;

  if( !label_decls.empty() )
    {
    tree switch_statement_list = make_node(STATEMENT_LIST);
    TREE_TYPE(switch_statement_list) = void_type_node;

    tree switchexpr = build2(SWITCH_EXPR,
                             integer_type_node,
                             proc->alter_index,
                             switch_statement_list);
    gg_append_statement(switchexpr);
    current_function->statement_list_stack.push_back(switch_statement_list);

    tree caselabel;
    tree labeldecl;

    for(size_t i = 0; i < label_decls.size()+1; ++i)
      {
      // Start with the case label for the pseudo-return location.
      tree val =
            build_int_cst(TREE_TYPE(proc->alter_index), i);

      labeldecl = create_artificial_label(UNKNOWN_LOCATION);
      DECL_CONTEXT(labeldecl) = current_function->function_decl;

      caselabel = build_case_label(val,
                                   NULL_TREE,
                                   labeldecl);
      gg_append_statement(caselabel);

      // And follow up with a goto expression for the pseudo-return location.
      if( i == 0 )
        {
#if 0
        fprintf(stderr,
                "build_alter_switch(1) for %s %s %p\n",
                proc->label->name,
                label_decl_text_from_expr(proc->no_alter_goto),
                (void *)GOTO_DESTINATION(proc->no_alter_goto));
#endif
        gg_append_statement(proc->no_alter_goto);
        }
      else
        {
        tree goto_expr  = build1( GOTO_EXPR,
                                  void_type_node,
                                  label_decls[i-1]);
        gg_append_statement(goto_expr);
        }
      }

    // End with a fall-through with "default:" case
    labeldecl = create_artificial_label(UNKNOWN_LOCATION);
    DECL_CONTEXT(labeldecl) = current_function->function_decl;
    caselabel = build_case_label(NULL_TREE,
                                 NULL_TREE,
                                 labeldecl);
    gg_append_statement(caselabel);

    current_function->statement_list_stack.pop_back();
    }
#if 0
  fprintf(stderr,
          "build_alter_switch(2) for %s %s %p\n",
          proc->label->name,
          label_decl_text_from_expr(proc->no_alter_goto),
          (void *)GOTO_DESTINATION(proc->no_alter_goto));
#endif
  gg_append_statement(proc->no_alter_goto);
  }

static void
build_entry_switch(const std::vector<tree> &goto_expr)
  {
  // This routine accepts a vector of GOTO_EXPRs.  It lays down code
  // equivalent to
  //    if( goto_expr.size() )
  //      {
  //      switch(var_decl_entry_index)
  //         {
  //         case 1:
  //            var_decl_entry_index = 0
  //            goto goto_expr[0]
  //         ...
  //         case N:
  //            var_decl_entry_index = 0
  //            goto goto_expr[N-1];
  //         default:
  //            abort();
  //         }
  //       }

  if( !goto_expr.empty() )
    {
    tree switch_statement_list = make_node(STATEMENT_LIST);
    TREE_TYPE(switch_statement_list) = void_type_node;

    tree switchexpr = build2(SWITCH_EXPR,
                             integer_type_node,
                             var_decl_entry_index,
                             switch_statement_list);
    gg_append_statement(switchexpr);
    current_function->statement_list_stack.push_back(switch_statement_list);

    tree caselabel;
    tree labeldecl;

    for(size_t i = 0; i < goto_expr.size(); ++i)
      {
      // Start with the case label for the pseudo-return location.
      tree val = build_int_cst(SIZE_T, i+1);

      labeldecl = create_artificial_label(UNKNOWN_LOCATION);
      DECL_CONTEXT(labeldecl) = current_function->function_decl;

      caselabel = build_case_label(val,
                                   NULL_TREE,
                                   labeldecl);
      gg_append_statement(caselabel);

      // Each case starts out by zeroing the global index:
      gg_assign(var_decl_entry_index, size_t_zero_node);
      // Followed by the goto
      gg_append_statement(goto_expr[i]);
      }

    // End with a default: case specifying an abort();
    labeldecl = create_artificial_label(UNKNOWN_LOCATION);
    DECL_CONTEXT(labeldecl) = current_function->function_decl;
    caselabel = build_case_label(NULL_TREE,
                                 NULL_TREE,
                                 labeldecl);
    gg_append_statement(caselabel);
    gg_abort();

    current_function->statement_list_stack.pop_back();
    }
  }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
static void
build_perform_dispatcher()
  {
  // This routine lays down the dispatcher that handles the return from
  // PERFORM <proc>

  // We need to create an execution island.  The switch() statement will
  // live on it.

  // Create the GOTO and the LABEL for this island
  tree island_goto;
  tree island_label;
  gg_create_goto_pair(&island_goto, &island_label);
  // GOTO the far side of the island.
  gg_append_statement(island_goto);

  // We need to build N switch statements, one for each paragraph that was
  // the target of a perform:

  // The list is a vector<void *>
  for( auto it : current_function->list_of_procedures )
    {
    cbl_proc_t *proc = static_cast<cbl_proc_t *>(it);
    // Each switch statement is the target of a GOTO at the end of a
    // paragraph.  In the case of a paragraph that was never called, the
    // code targeting the label will never be executed; the GOTO will always
    // be skipped by the end-of-paragraph code checking the top of the pseudo-
    // return stack.  But we need the label anyway, because otherwise the
    // middle-end Control Flow Graph CFG processing crashes.
    gg_append_statement(proc->dispatch_switch_label);

    // And after each such label, the switch statement:
    build_dispatch_switch(proc->pseudo_return_decls);

    // Do something similar for ALTER
    gg_append_statement(proc->alter_switch_label);
    // And after each such label, the switch statement:
    build_alter_switch(proc, proc->alter_decls);
    }
  // Do something similar for ENTER
  tree label = current_function->entry_switch_label;
  gg_append_statement(label);
  // And after each such label, the switch statement:
  build_entry_switch(current_function->entry_goto_expressions);

  // Lay down the label for jumping over the island.
  gg_append_statement(island_label);
  }
#pragma GCC diagnostic pop

void
parser_end_program(const char *prog_name )
  {
  if( gg_trans_unit.function_stack.size() )
    {
    // The body has been created by various parser calls.  It's time
    // to wrap this sucker up!

    // Ending the program ends the current paragraph and section:
    leave_paragraph_internal();
    leave_section_internal();
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    TRACE1_TEXT_ABC("\"", prog_name, "\"")
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("\"", prog_name, "\"")
    TRACE1_END
    }

  if( ! label_verify.vet() )
    {
    label_verify.dump();
    gcc_unreachable();
    }

  if( !hijacked )
    {
    build_perform_dispatcher();
    }

  if( gg_trans_unit.function_stack.size() )
    {
    // The body has been created by various parser calls.  It's time
    // to wrap this sucker up!

    // Put in a harmless return in case there was no EXIT PROGRAM statement.
    // It's harmless because if it isn't needed, a return was already
    // executed, and this generated code will never be executed
    parser_exit( cbl_refer_t() );

    // Tell the GCC compiler to do the GIMPLIFY thing.
    gg_finalize_function();
    }
  }

static void
remove_p_from_picture(char *picture)
  {
  // At this point, attr has the scaled_e flag, and rdigits tells us
  // which way to scale.  So, the P characters in picture are now
  // a liability.

  char *rabbit = picture;
  char *fox = picture;

  for(;;)
    {
    char ch = *rabbit++;
    if( ch == '\0' )
      {
      break;
      }
    if( ch == 'P' || ch == 'p' )
      {
      if( *rabbit == '(' )
        {
        while( *rabbit != ')' )
          {
          rabbit += 1;
          }
        rabbit += 1;
        // rabbit now points to one past the closing parenthesis
        }
      size_t to_move = strlen(rabbit);
      memmove(fox, rabbit, to_move+1);  // +1 snags the '\0'
      rabbit = fox;
      }
    else
      {
      fox += 1;
      }
    }
  }

static tree vti_array;
static tree vti_constructor;
static int  vti_list_size;
static int  vti_next_variable;

void
parser_init_list_size(int count_of_variables)
  {
  if( mode_syntax_only() ) return;

  vti_list_size = count_of_variables;
  char ach[48];
  sprintf(ach,
          "..variables_to_init_" HOST_SIZE_T_PRINT_DEC,
          (fmt_size_t)current_function->our_symbol_table_index);
  tree array_of_variables_type = build_array_type_nelts(VOID_P,
                                                        count_of_variables+1);
  vti_array = gg_define_variable( array_of_variables_type,
                                  ach,
                                  vs_file_static);
  vti_constructor                 = make_node(CONSTRUCTOR);
  TREE_TYPE(vti_constructor)      = array_of_variables_type;
  TREE_STATIC(vti_constructor)    = 1;
  TREE_CONSTANT(vti_constructor)  = 1;
  vti_next_variable = 0;
  }

void
parser_init_list_element(cbl_field_t *field)
  {
  if( mode_syntax_only() ) return;

  gcc_assert(vti_next_variable < vti_list_size);
  CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(vti_constructor),
                          build_int_cst_type(SIZE_T, vti_next_variable++),
                          gg_get_address_of(field->var_decl_node) );
  if( vti_next_variable == vti_list_size)
    {
    CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(vti_constructor),
                            build_int_cst_type(SIZE_T, vti_next_variable++),
                            null_pointer_node );
    DECL_INITIAL(vti_array) = vti_constructor;
    }
  }

void
parser_init_list()
  {
  if( mode_syntax_only() ) return;

  RETURN_WHEN_HIJACKED;

  char ach[48];
  sprintf(ach,
          "..variables_to_init_" HOST_SIZE_T_PRINT_DEC,
          (fmt_size_t)current_function->our_symbol_table_index);
  tree array = gg_trans_unit_var_decl(ach);

  int flag_bits =  wsclear()
                ? DEFAULTBYTE_BIT + (*wsclear() & DEFAULT_BYTE_MASK)
                : 0;
  gg_call(VOID,
          "__gg__variables_to_init",
          gg_pointer_to_array(array),
          build_int_cst_type(INT, flag_bits),
          NULL_TREE);
  }

static
FIXED_WIDE_INT(128)
dirty_to_binary(const char  *instring,
                uint32_t    &capacity,
                uint32_t    &digits,
                int32_t     &rdigits,
                uint64_t    &attr)
  {
  digits = 0;
  rdigits = 0;
  attr = 0;

  FIXED_WIDE_INT(128) value = 0;

  // We need to convert data.initial to an FIXED_WIDE_INT(128) value
  const char *p = instring;
  int sign = 1;
  if( *p == '-' )
    {
    attr |= signable_e;
    sign = -1;
    p += 1;
    }
  else if( *p == '+' )
    {
    // We set it signable so that the instruction DISPLAY +1
    // actually outputs "+1"
    attr |= signable_e;
    p += 1;
    }

  //  We need to be able to handle
  //  123
  //  123.456
  //  123E<exp>
  //  123.456E<exp>
  //  where <exp> can be N, +N and -N
  //
  //  Oh, yeah, and we're talking handling up to 32 digits, or more, so using
  //  library routines is off the table.

  int rdigit_delta = 0;
  int exponent = 0;
  const char *exp = strchr(p, 'E');
  if( !exp )
    {
    exp = strchr(p, 'e');
    }
  if(exp)
    {
    exponent = atoi(exp+1);
    }

  // We can now calculate the value, and the number of digits and rdigits.

  // We count up leading zeroes as part of the attr->digits calculation.
  // It turns out that certain comparisons need to know the number of digits,
  // because "IF "2" EQUAL 002" is false, while "IF "2" EQUAL 2" is true.  So,
  // we need to count up leading zeroes.

  for(;;)
    {
    char ch = *p++;
    if( ch == symbol_decimal_point() )
      {
      rdigit_delta = 1;
      continue;
      }
    if( ch < '0' || ch > '9' )
      {
      break;
      }
    digits += 1;
    rdigits += rdigit_delta;
    value *= 10;
    value += ch - '0';
    }

  if( exponent < 0 )
    {
    rdigits += -exponent;
    }
  else
    {
    while(exponent--)
      {
      if(rdigits)
        {
        rdigits -= 1;
        }
      else
        {
        digits += 1;
        value *= 10;
        }
      }
    }

  if( (int32_t)digits < rdigits )
    {
    digits = rdigits;
    }

  // We now need to calculate the capacity.

  unsigned int min_prec = wi::min_precision(value, UNSIGNED);
  if( min_prec > 64 )
    {
    // Bytes 15 through 8 are non-zero
    capacity = 16;
    }
  else if( min_prec > 32 )
    {
    // Bytes 7 through 4 are non-zero
    capacity = 8;
    }
  else if( min_prec > 16 )
    {
    // Bytes 3 and 2
    capacity = 4;
    }
  else if( min_prec > 8 )
    {
    // Byte 1 is non-zero
    capacity = 2;
    }
  else
    {
    // The value is zero through 0xFF
    capacity = 1;
    }

  value *= sign;

  // One last adjustment.  The number is signable, so the binary value
  // is going to be treated as twos complement.  That means that the highest
  // bit has to be 1 for negative signable numbers, and 0 for positive.  If
  // necessary, adjust capacity up by one byte so that the variable fits:

  if( capacity < 16 && (attr & signable_e) )
    {
    FIXED_WIDE_INT(128) mask
      = wi::set_bit_in_zero<FIXED_WIDE_INT(128)>(capacity * 8 - 1);
    if( wi::neg_p (value) && (value & mask) == 0 )
      {
      capacity *= 2;
      }
    else if( !wi::neg_p (value) && (value & mask) != 0 )
      {
      capacity *= 2;
      }
    }

  return value;
  }


static void
psa_FldLiteralN(struct cbl_field_t *field )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", field)
    SHOW_PARSE_END
    }
  // We are constructing a completely static constant structure, based on the
  // text string in .initial

  CHECK_FIELD(field);

  uint32_t capacity;
  uint32_t digits;
  int32_t  rdigits;
  uint64_t attr;
  FIXED_WIDE_INT(128) value = dirty_to_binary(field->data.original(),
                                              capacity,
                                              digits,
                                              rdigits,
                                              attr);
  // This is a rare occurrence of a parser_xxx call changing the entry
  // in the symbol table.
  field->data.capacity(  capacity );
  field->data.digits   = digits;
  field->data.rdigits  = rdigits;
  field->attr         |= attr;

  char base_name[257];
  char id_string[32] = "";

  static size_t our_index = 0;

  sprintf(id_string, "." HOST_SIZE_T_PRINT_DEC, (fmt_size_t)++our_index);
  strcpy(base_name, field->name);
  strcat(base_name, id_string);

  tree var_type;

  // The value is 1, 2, 4, 8 or 16 bytes, so an ordinary constructor can be
  // used.
  var_type = tree_type_from_field(field);
  tree new_var_decl = gg_define_variable( var_type,
                                          base_name,
                                          vs_static);
  DECL_INITIAL(new_var_decl)  = wide_int_to_tree(var_type, value);
  TREE_CONSTANT(new_var_decl) = 1;
  TREE_READONLY(new_var_decl) = 1;

  field->data_decl_node = new_var_decl;

  // Note that during compilation, the integer value, assuming it can be
  // contained in 128-bit integers, can be accessed with
  //
  //  wi::to_wide( DECL_INITIAL(new_var_decl) )
  }

void
parser_accept(const struct cbl_refer_t &tgt,
              special_name_t special_e,
              cbl_label_t *error,
              cbl_label_t *not_error )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( error )
      {
      SHOW_PARSE_LABEL(" error ", error)
      }
    if( not_error )
      {
      SHOW_PARSE_LABEL(" not_error ", not_error)
      }
    SHOW_PARSE_END
    }

  // The ISO spec describes the valid special names for ACCEPT as implementation
  // dependent.  We are following IBM's lead.

  tree environment = build_int_cst_type(INT, special_e);

  const char *function_to_call = NULL;

  switch(special_e)
    {
    case STDIN_e:
    case CONSOLE_e:
    case SYSIPT_e:
    case SYSIN_e:
      // This is ordinary input from from the stdin:
      gg_call(VOID,
              "__gg__accept",
              environment,
              gg_get_address_of(tgt.field->var_decl_node),
              refer_offset(tgt),
              refer_size_dest(tgt),
              NULL_TREE);
      return;
      break;

    case C01_e:
    case C02_e:
    case C03_e:
    case C04_e:
    case C05_e:
    case C06_e:
    case C07_e:
    case C08_e:
    case C09_e:
    case C10_e:
    case C11_e:
    case C12_e:
    case CSP_e:
    case S01_e:
    case S02_e:
    case S03_e:
    case S04_e:
    case S05_e:
    case AFP_5A_e:
    case STDOUT_e:
    case SYSOUT_e:
    case SYSLIST_e:
    case SYSLST_e:
    case STDERR_e:
    case SYSPUNCH_e:
    case SYSPCH_e:
    case SYSERR_e:
      cbl_internal_error("Not valid for ACCEPT statement.");
      break;

    case ARG_NUM_e:
      // This ACCEPT statement wants the number of argv values:
      gg_call(VOID,
              "__gg__get_argc",
              gg_get_address_of(tgt.field->var_decl_node),
              refer_offset(tgt),
              refer_size_source(tgt),
              NULL_TREE);
       return;
       break;

    case ENV_NAME_e:
      // This fetches the environment name set by DISPLAY... UPON ENV_NAME_e
      gg_call(VOID,
              "__gg__get_env_name",
              gg_get_address_of(tgt.field->var_decl_node),
              refer_offset(tgt),
              refer_size_source(tgt),
              NULL_TREE);
       return;
       break;

    case ENV_VALUE_e:
      // This fetches the environment value associated with the previously
      // esablished name
      function_to_call = "__gg__get_env_value";
      break;

    case ARG_VALUE_e:
      // We are fetching the variable whose index was established by a prior
      // DISPLAY UPON ARGUMENT-NUMBER.  After the fetch, the value will be
      // incremented by one.
      function_to_call = "__gg__accept_arg_value";
      break;
    }
  if( function_to_call )
    {
    tree erf = gg_define_variable(INT);
    gg_assign(erf,
              gg_call_expr(  INT,
                            function_to_call,
                            gg_get_address_of(tgt.field->var_decl_node),
                            refer_offset(tgt),
                            refer_size_dest(tgt),
                            NULL_TREE));
    if( error )
      {
      // There is an ON EXCEPTION phrase:
      IF( erf, ne_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO     error->INTO for_argv")
          SHOW_PARSE_LABEL_OK(" ", error)
          }
        gg_append_statement( error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    if( not_error )
      {
      // There is an NOT ON EXCEPTION phrase:
      IF( erf, eq_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO not_error->INTO for_argv")
          SHOW_PARSE_LABEL_OK(" ", not_error)
          }
        gg_append_statement( not_error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    if( error )
      {
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("Laying down LABEL     error->bottom")
        SHOW_PARSE_LABEL_OK(" ", error)
        }
      gg_append_statement( error->structs.arith_error->bottom.label );
      }
    if( not_error )
      {
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("Laying down LABEL not_error->bottom")
        SHOW_PARSE_LABEL_OK(" ", not_error)
        SHOW_PARSE_END
        }
      gg_append_statement( not_error->structs.arith_error->bottom.label );
      }
    }
  }

// TODO: update documentation.
void
parser_accept_exception( cbl_label_t *accept_label )
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We are entering either SIZE ERROR or NOT SIZE ERROR code
  RETURN_IF_PARSE_ONLY;

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO OVER")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("Laying down LABEL INTO:")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_END
    }

  CHECK_LABEL(accept_label);
  set_up_on_exception_label(accept_label);

  // Jump over the [NOT] ON EXCEPTION code that is about to be laid down
  gg_append_statement( accept_label->structs.arith_error->over.go_to );
  // Create the label that allows the following code to be executed at
  // when an ERROR, or NOT ERROR, has been determined to have taken place:
  gg_append_statement( accept_label->structs.arith_error->into.label );
  }

void
parser_accept_exception_end( cbl_label_t *accept_label )
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We have reached the end of the ERROR, or NOT ERROR, code.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO BOTTOM")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("Laying down LABEL OVER:")
    SHOW_PARSE_LABEL(" ", accept_label)
    SHOW_PARSE_END
    }

  CHECK_LABEL(accept_label);

  // Jump to the end of the arithmetic code:
  gg_append_statement( accept_label->structs.arith_error->bottom.go_to );
  // Lay down the label that allows the ERROR/NOT ERROR instructions
  // to exist in a lacuna that doesn't get executed unless somebody jumps
  // to it:
  gg_append_statement( accept_label->structs.arith_error->over.label );
  }

void
parser_accept_command_line( const cbl_refer_t &tgt,
                            const cbl_refer_t &source,
                            cbl_label_t *error,
                            cbl_label_t *not_error )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( error )
      {
      SHOW_PARSE_LABEL(" error ", error)
      }
    if( not_error )
      {
      SHOW_PARSE_LABEL(" not_error ", not_error)
      }
    SHOW_PARSE_END
    }

  tree erf = gg_define_variable(INT);

  if( !source.field )
    {
    // The whole command-line is wanted
    gg_assign(erf,
              gg_call_expr( INT,
                            "__gg__get_command_line",
                            gg_get_address_of(tgt.field->var_decl_node),
                            refer_offset(tgt),
                            refer_size_dest(tgt),
                            NULL_TREE));
    if( error )
      {
      // There is an ON EXCEPTION phrase:
      IF( erf, ne_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO     error->INTO for_command_line")
          SHOW_PARSE_LABEL_OK(" ", error)
          }
        gg_append_statement( error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    if( not_error )
      {
      // There is an NOT ON EXCEPTION phrase:
      IF( erf, eq_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO not_error->INTO for command_line")
          SHOW_PARSE_LABEL_OK(" ", not_error)
          }
        gg_append_statement( not_error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    }
  else
    {
    // A particular parameter has been requested:
    gg_assign(erf,
              gg_call_expr(  INT,
                            "__gg__get_argv",
                            gg_get_address_of(tgt.field->var_decl_node),
                            refer_offset(tgt),
                            refer_size_dest(tgt),
                            gg_get_address_of(source.field->var_decl_node),
                            refer_offset(source),
                            refer_size_dest(source),
                            NULL_TREE));
    if( error )
      {
      // There is an ON EXCEPTION phrase:
      IF( erf, ne_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO     error->INTO for_argv")
          SHOW_PARSE_LABEL_OK(" ", error)
          }
        gg_append_statement( error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    if( not_error )
      {
      // There is an NOT ON EXCEPTION phrase:
      IF( erf, eq_op, integer_zero_node )
        {
        SHOW_PARSE
          {
          SHOW_PARSE_INDENT
          SHOW_PARSE_TEXT("Laying down GOTO not_error->INTO for_argv")
          SHOW_PARSE_LABEL_OK(" ", not_error)
          }
        gg_append_statement( not_error->structs.arith_error->into.go_to );
        }
      ELSE
        {
        }
        ENDIF
      }
    }
  if( error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL     error->bottom")
      SHOW_PARSE_LABEL_OK(" ", error)
      }
    gg_append_statement( error->structs.arith_error->bottom.label );
    }
  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL not_error->bottom")
      SHOW_PARSE_LABEL_OK(" ", not_error)
      SHOW_PARSE_END
      }
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }
  }

void
parser_accept_command_line_count( const cbl_refer_t &tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  gg_call(  VOID,
            "__gg__get_argc",
            gg_get_address_of(tgt.field->var_decl_node),
            refer_offset(tgt),
            refer_size_dest(tgt),
            NULL_TREE);
  }

void
parser_accept_envar(const struct cbl_refer_t &tgt,
                    const struct cbl_refer_t &envar,
                          cbl_label_t *error,
                          cbl_label_t *not_error )
  {
  Analyze();

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( error )
      {
      SHOW_PARSE_LABEL(" error ", error)
      }
    if( not_error )
      {
      SHOW_PARSE_LABEL(" not_error ", not_error)
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  tree erf = gg_define_variable(INT);

  gg_assign(erf,
            gg_call_expr( INT,
                          "__gg__accept_envar",
                          gg_get_address_of(tgt.field->var_decl_node),
                          refer_offset(tgt),
                          refer_size_dest(tgt),
                          gg_get_address_of(envar.field->var_decl_node),
                          refer_offset(envar),
                          refer_size_source(envar),
                          NULL_TREE));
  if( error )
    {
    // There is an ON EXCEPTION phrase:
    IF( erf, ne_op, integer_zero_node )
      {
      gg_append_statement( error->structs.arith_error->into.go_to );
      }
    ELSE
      {
      }
      ENDIF
    }
  if( not_error )
    {
    // There is an NOT ON EXCEPTION phrase:
    IF( erf, eq_op, integer_zero_node )
      {
      gg_append_statement( not_error->structs.arith_error->into.go_to );
      }
    ELSE
      {
      }
      ENDIF
    }
  if( error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL     error->bottom")
      SHOW_PARSE_LABEL_OK(" ", error)
      }
    gg_append_statement( error->structs.arith_error->bottom.label );
    }
  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("Laying down LABEL not_error->bottom")
      SHOW_PARSE_LABEL_OK(" ", not_error)
      SHOW_PARSE_END
      }
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }
  }

void
parser_set_envar( const struct cbl_refer_t &name,
                  const struct cbl_refer_t &value )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  // Set name to value using setenv(3)
  gg_call(BOOL,
          "__gg__set_envar",
          gg_get_address_of(name.field->var_decl_node),
          refer_offset(name),
          refer_size_source(name),
          gg_get_address_of(value.field->var_decl_node),
          refer_offset(value),
          refer_size_source(value),
          NULL_TREE);
  }

void
parser_accept_date_yymmdd( const cbl_refer_t& refer )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  auto target = refer.field;
  CHECK_FIELD(target);

  gg_call(VOID,
          "__gg__get_date_yymmdd",
          gg_get_address_of(target->var_decl_node),
          refer_offset(refer),
          refer_size_dest(refer),
          NULL_TREE);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_yyyymmdd( const cbl_refer_t& refer )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  auto target = refer.field;
  gg_call(VOID,
          "__gg__get_date_yyyymmdd",
          gg_get_address_of(target->var_decl_node),
          refer_offset(refer),
          refer_size_dest(refer),
          NULL_TREE);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_yyddd( const cbl_refer_t& refer )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  auto target = refer.field;
  CHECK_FIELD(target);

  gg_call(VOID,
          "__gg__get_date_yyddd",
          gg_get_address_of(target->var_decl_node),
          refer_offset(refer),
          refer_size_dest(refer),
          NULL_TREE);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target,"");
    TRACE1_END
    }
  }

void
parser_accept_date_yyyyddd( const cbl_refer_t& refer )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  auto target = refer.field;
  CHECK_FIELD(target);

  gg_call(VOID,
          "__gg__get_yyyyddd",
          gg_get_address_of(target->var_decl_node),
          refer_offset(refer),
          refer_size_dest(refer),
          NULL_TREE);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_dow( const cbl_refer_t& refer )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  auto target = refer.field;
  CHECK_FIELD(target);

  gg_call(VOID,
          "__gg__get_date_dow",
          gg_get_address_of(target->var_decl_node),
          refer_offset(refer),
          refer_size_dest(refer),
          NULL_TREE);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

void
parser_accept_date_hhmmssff( const cbl_refer_t& refer )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  auto target = refer.field;
  CHECK_FIELD(target);

  gg_call(VOID,
          "__gg__get_date_hhmmssff",
          gg_get_address_of(target->var_decl_node),
          refer_offset(refer),
          refer_size_dest(refer),
          NULL_TREE);
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("", target, "")
    TRACE1_END
    }
  }

/*
 * If the encoding is anything but custom, the enumerated type
 * cbl_encoding_t suffices to describe it.  At least for now, the rest
 * of cbl_alphabet_t in those cases is unused.
 *
 * To get the symbol index: symbol_index(symbol_elem_of(&alphabet))
 *
 * The parameter is always a reference to an element in the symbol table.
 */

void
parser_alphabet( const cbl_alphabet_t *alphabet )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char *psz = xasprintf(" %s ", alphabet->name);
    SHOW_PARSE_TEXT(psz);
    free(psz);
    switch(alphabet->encoding)
      {
      case iconv_CP1252_e:
        psz = xasprintf("CP1252");
        break;
      case ASCII_e:
        psz = xasprintf("ASCII");
        break;
      case iso646_e:
        psz = xasprintf("ISO646");
        break;
      case EBCDIC_e:
        psz = xasprintf("EBCDIC");
        break;
      case UTF8_e:
        psz = xasprintf("UTF8");
        break;
      case custom_encoding_e:
        psz = xasprintf("%s", alphabet->name);
        break;
      default:
        { const char * p = __gg__encoding_iconv_name( alphabet->encoding );
          psz = xasprintf("%s", p? p : "[unknown]");
        }
      }
    SHOW_PARSE_TEXT(" ");
    SHOW_PARSE_TEXT(psz);
    free(psz);
    SHOW_PARSE_END
    }

  switch(alphabet->encoding)
    {
    case iconv_CP1252_e:
    case ASCII_e:
    case iso646_e:
    case EBCDIC_e:
    case UTF8_e:
      break;

    case custom_encoding_e:
      {
      uint64_t alphabet_index = symbol_unique_index(symbol_elem_of(alphabet));

      unsigned char ach[256];

      tree table_type = build_array_type_nelts(UCHAR, 256);
      tree table256   = gg_define_variable(table_type);
      for( int i=0; i<256; i++ )
        {
        // character i has the ordinal alphabet[i]
        unsigned char ch = i;

        ach[ch] = (alphabet->collation_sequence[i]);
        gg_assign(  gg_array_value(table256, ch),
                    build_int_cst_type(UCHAR, (alphabet->collation_sequence[i])) );
        }

      unsigned int low_char  = alphabet->low_char;
      unsigned int high_char = alphabet->high_char;
      __gg__alphabet_create(alphabet->encoding,
                            alphabet_index,
                            ach,
                            low_char,
                            high_char);
      gg_call(VOID,
              "__gg__alphabet_create",
              build_int_cst_type(INT, alphabet->encoding),
              build_int_cst_type(SIZE_T, alphabet_index),
              gg_pointer_to_array(table256),
              build_int_cst_type(INT, low_char),
              build_int_cst_type(INT, high_char),
              NULL_TREE );
      break;
      }
    default:
      fprintf(stderr, "%s: Program ID %s:\n",
              cobol_filename(),
              cbl_label_of(symbol_at(current_program_index()))->name);
      gcc_unreachable();
    }
  }

void
parser_alphabet_use( const cbl_alphabet_t *alphabet )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char *psz = xasprintf(" %s ", alphabet->name);
    SHOW_PARSE_TEXT(psz);
    free(psz);
    switch(alphabet->encoding)
      {
      case iconv_CP1252_e:
        psz = xasprintf("CP1252");
        break;
      case ASCII_e:
        psz = xasprintf("ASCII");
        break;
      case iso646_e:
        psz = xasprintf("ISO646");
        break;
      case EBCDIC_e:
        psz = xasprintf("EBCDIC");
        break;
      case UTF8_e:
        psz = xasprintf("UTF8");
        break;
      case custom_encoding_e:
        psz = xasprintf("%s", alphabet->name);
        break;
      default:
        gcc_unreachable();
      }
    SHOW_PARSE_TEXT(psz);
    free(psz);
    SHOW_PARSE_END
    }

  uint64_t alphabet_index = symbol_unique_index(symbol_elem_of(alphabet));

  current_function->alphabet_in_use = true;

  switch(alphabet->encoding)
    {
    default:
      gcc_unreachable();
    case iconv_CP1252_e:
    case ASCII_e:
    case iso646_e:
    case EBCDIC_e:
    case UTF8_e:
      __gg__low_value_character  = DEGENERATE_LOW_VALUE;
      __gg__high_value_character = DEGENERATE_HIGH_VALUE;
      gg_call(VOID,
              "__gg__alphabet_use",
              build_int_cst_type(INT, current_encoding(display_encoding_e)),
              build_int_cst_type(INT, current_encoding(national_encoding_e)),
              build_int_cst_type(INT, alphabet->encoding),
              null_pointer_node,
              NULL_TREE);
      break;

    case custom_encoding_e:
      std::unordered_map<size_t, alphabet_state>::const_iterator it =
        __gg__alphabet_states.find(alphabet_index);

      assert( it != __gg__alphabet_states.end());
      __gg__low_value_character  = it->second.low_char;
      __gg__high_value_character = it->second.high_char;

      gg_call(VOID,
              "__gg__alphabet_use",
              build_int_cst_type(INT, current_encoding(display_encoding_e)),
              build_int_cst_type(INT, current_encoding(national_encoding_e)),
              build_int_cst_type(INT, alphabet->encoding),
              build_int_cst_type(SIZE_T, alphabet_index),
              NULL_TREE);
      break;
    }
  }

void
parser_display_literal(const char *literal, bool advance)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" \"");
    SHOW_PARSE_TEXT(literal)
    SHOW_PARSE_TEXT("\"");
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("About to DISPLAY a literal:")
    TRACE1_END
    }

  tree file_descriptor = integer_one_node; // Just stdout, for now
  gg_write(  file_descriptor,
             gg_string_literal(literal),
             build_int_cst_type(integer_type_node,(int)strlen(literal)) );

  if( advance )
    {
    gg_write(  file_descriptor,
               gg_string_literal("\n"),
               integer_one_node);
    }
  cursor_at_sol = advance;
  }

void
parser_display_internal(tree file_descriptor,
                  const cbl_refer_t &refer,
                        bool advance)
  {
  Analyze();
  if( refer.field->type == FldConditional )
    {
    TRACE1
      {
      gg_create_true_false_statement_lists(refer.field->var_decl_node);
        gg_fprintf(file_descriptor, 0, "TRUE");
      ELSE
        gg_fprintf(file_descriptor, 0, "FALSE");
      ENDIF
      }
    }
  else if( refer.field->type == FldLiteralN )
    {
    // The parser found the string of digits from the source code and converted
    // it to a 128-bit binary floating point number.

    // The bad news is that something like 555.55 can't be expressed exactly;
    // internally it is 555.5499999999....

    // The good news is that we know any string of 33 or fewer decimal digits
    // can be converted to and from IEEE 754 binary128 without being changes

    // We make use of that here

    char ach[128];
    real_to_decimal (ach, TREE_REAL_CST_PTR (refer.field->data.value_of()),
                     sizeof(ach), 33, 0);
    char *p = strchr(ach, 'e');
    if( !p )
      {
      // Probably INF -INF NAN or -NAN, so ach has our result
      // Except that real_to_decimal prints -0.0 and 0.0 like that with
      // no e.
      if( ach[0] == '0' || ( ach[0] == '-' && ach[1] == '0' ))
        __gg__remove_trailing_zeroes(ach);
      }
    else
      {
      int exp = atoi(p+1);
      if( exp >= 6 || exp <= -5 )
        {
        // We are going to stick with the E notation, so ach has our result
        // Except that real_to_decimal prints with e notation rather than E
        // and doesn't guarantee at least two exponent digits.
        *p = 'E';
        if( exp < 0 && exp >= -9 )
          {
          p[1] = '-';
          p[2] = '0';
          p[3] = '0' - exp;
          p[4] = '\0';
          }
        else if( exp >= 0 && exp <= 9 )
          {
          p[1] = '+';
          p[2] = '0';
          p[3] = '0' + exp;
          p[4] = '\0';
          }
        }
      else if (exp == 0)
        {
        p[-1] = '\0';
        }
      else if (exp < 0)
        {
        p[-1] = '\0';
        char *q = strchr (ach, '.');
        char dig = q[-1];
        q[-1] = '\0';
        char tem[132];
        snprintf (tem, 132, "%s0.%0*d%c%s", ach, -exp - 1, 0, dig, q + 1);
        strcpy (ach, tem);
        }
      else // if (exp > 0)
        {
        p[-1] = '\0';
        char *q = strchr (ach, '.');
        for (int i = 0; i != exp; ++i)
          q[i] = q[i + 1];
        q[exp] = '.';
        }
      __gg__remove_trailing_zeroes(ach);
      }

    if( symbol_decimal_point() == ',' )
      {
      char *pdot = strchr(ach, '.' );
      if( pdot )
        {
        *pdot = symbol_decimal_point();
        }
      }

    gg_write(  file_descriptor,
               gg_string_literal(ach),
               build_int_cst_type(SIZE_T, strlen(ach)));
    if( advance )
      {
      gg_write(  file_descriptor,
                 gg_string_literal("\n"),
                 integer_one_node);
      }
    }
  else if(    refer.field->type == FldFloat
           && refer.field->attr & constant_e
           && !(refer.field->attr & intermediate_e) )
    {
    // We are going to output what we think the user typed in the first place
    char * const to_print = xstrdup(refer.field->data.original());
    char *p = to_print;
    if( *p == ascii_plus )
      {
      p += 1;
      }
    size_t len = strlen(p);
    if(len > 2 && p[len-2] == ascii_E && p[len-1] == ascii_0 )
      {
      len -= 2;
      }
    gg_write(  file_descriptor,
               build_string_literal(len, p),
               build_int_cst_type(SIZE_T, len));
    free(to_print);

    if( advance )
      {
      gg_write(  file_descriptor,
                 gg_string_literal("\n"),
                 integer_one_node);
      }
    }
  else
    {
    int flags  = advance ? 1 : 0;
        flags |= refer.addr_of ? REFER_T_ADDRESS_OF : 0;
    if( refer_is_clean(refer) )
      {
      gg_call(VOID,
              "__gg__display_clean",
              gg_get_address_of(refer.field->var_decl_node),
              file_descriptor,
              build_int_cst_type(INT, flags),
              NULL_TREE );
      }
    else
      {
      // We might be dealing with a refmod:
      if( refer.refmod.from || refer.refmod.len )
        {
        attribute_bit_set(refer.field, refmod_e);
        }
      gg_call(VOID,
              "__gg__display",
              gg_get_address_of(refer.field->var_decl_node),
              refer_offset(refer),
              refer_size_source(  refer),
              file_descriptor,
              build_int_cst_type(INT, flags),
              NULL_TREE );
      if( refer.refmod.from || refer.refmod.len )
        {
        attribute_bit_clear(refer.field, refmod_e);
        }
      }
    }
  cursor_at_sol = advance;
  }

void
parser_display_field(cbl_field_t *field)
  {
  parser_display_internal_field(integer_one_node,
                                field,
                                DISPLAY_NO_ADVANCE);
  }

void
parser_display( const struct cbl_special_name_t *upon,
          const std::vector<cbl_refer_t> &refs,
                bool advance,
          const cbl_label_t *not_error,
          const cbl_label_t *error )
  {
  const size_t n = refs.size();
  /*
   * The first parameter to parser_display is the "device" upon which to display
   * the data. Besides normal devices, these may include elements that define the
   * Unix command line and environment:
   *  1.  ARG_NUM_e, the ARGUMENT-NUMBER
   *  2.  ARG_VALUE_e, the ARGUMENT-VALUE
   *  3.  ENV_NAME_e, the ENVIRONMENT-NAME
   *  4.  ENV_VALUE_e, the ENVIRONMENT-VALUE
   * that need special care and feeding.
   */

  // At the present time, I am not sure what not_error and error are for
  gcc_assert(!not_error);
  gcc_assert(!error);

  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" parser_display of multiple variables:")
    for(size_t i=0; i<n; i++)
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_REF("", refs.at(i));
      }
    if( advance )
      {
      SHOW_PARSE_TEXT(" (advance)")
      }
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    for(size_t ii=0; ii<n; ii++)
      {
      if( ii != 0 )
        {
        TRACE1_INDENT
        }
      if(n > 1)
        {
        gg_fprintf(trace_handle, 1, "%ld: ", build_int_cst_type(INT, ii));
        }
      TRACE1_REFER("", refs[ii], "")
      }
    TRACE1_END
    }
  tree file_descriptor = gg_define_variable(INT);
  bool needs_closing = false;
  if( upon )
    {
    switch(upon->id)
      {
      // See table 5 in the IBM Cobol For Linux x86 1.2 document.

      case STDIN_e:
      case SYSIN_e:
      case SYSIPT_e:
        cbl_internal_error("Attempting to send to an input device.");
        break;

      case C01_e:
      case C02_e:
      case C03_e:
      case C04_e:
      case C05_e:
      case C06_e:
      case C07_e:
      case C08_e:
      case C09_e:
      case C10_e:
      case C11_e:
      case C12_e:
      case CSP_e:
      case S01_e:
      case S02_e:
      case S03_e:
      case S04_e:
      case S05_e:
      case AFP_5A_e:
      case ARG_VALUE_e:
        cbl_internal_error("Not valid for DISPLAY statement.");
        break;

      case STDOUT_e:
      case CONSOLE_e:
        // These are inarguably stdout
        gg_assign(file_descriptor, integer_one_node);
        break;

      case STDERR_e:
      case SYSERR_e:
        // These are inarguably stderr
        gg_assign(file_descriptor, integer_two_node);
        break;

      case SYSOUT_e:
      case SYSLIST_e:
      case SYSLST_e:
      case SYSPCH_e:
        // In the 21st century, when there are no longer valid assumptions to
        // be made about the existence of line printers, and where things
        // formerly-ubiquitous card punches no longer exist, there is a need
        // for the possibility of assigning these "devices" to externally-
        // determined Unix gadgetry in /dev:
        gg_assign(file_descriptor,
                  gg_call_expr( INT,
                                "__gg__get_file_descriptor",
                                gg_string_literal(upon->os_filename),
                                NULL_TREE));
        needs_closing = true;
        break;

      case SYSPUNCH_e:
        // With the ASSEMBLER environment variable, SYSPUNCH means "insert
        // the text into the assembly language".  So, we don't need a file
        // descriptor.
        if( !getenv("ASSEMBLER") )
          {
          gg_assign(file_descriptor,
                    gg_call_expr( INT,
                                  "__gg__get_file_descriptor",
                                  gg_string_literal(upon->os_filename),
                                  NULL_TREE));
          needs_closing = true;
          }
        break;


      case ARG_NUM_e:
        // Set the index number for a subsequent ACCEPT FROM ARG_VALUE_e
        gg_call(VOID,
                "__gg__set_arg_num",
                gg_get_address_of(refs[0].field->var_decl_node),
                refer_offset(refs[0]),
                refer_size_source(refs[0]),
                NULL_TREE);
         return;
         break;

      case ENV_NAME_e:
        // Establish the name of an environment variable for later use with
        // in either DISPLAY UPON or ACCEPT FROM
        gg_call(VOID,
                "__gg__set_env_name",
                gg_get_address_of(refs[0].field->var_decl_node),
                refer_offset(refs[0]),
                refer_size_source(refs[0]),
                NULL_TREE);
         return;
         break;

      case ENV_VALUE_e:
        // Set the contents of the environment variable named with ENV_NAME_e
        gg_call(VOID,
                "__gg__set_env_value",
                gg_get_address_of(refs[0].field->var_decl_node),
                refer_offset(refs[0]),
                refer_size_source(refs[0]),
                NULL_TREE);
         return;
         break;
      }
    }
  else
    {
    // stdout is file descriptor 1.
    gg_assign(file_descriptor, integer_one_node);
    }

  for(size_t i=0; i<n-1; i++)
    {
    CHECK_FIELD(refs[i].field);
    parser_display_internal(file_descriptor, refs[i], DISPLAY_NO_ADVANCE);
    }
  CHECK_FIELD(refs[n-1].field);

  if(    upon
      && upon->id == SYSPUNCH_e
      && getenv("ASSEMBLER")
      && refs[n-1].field
      && refs[n-1].field->type == FldLiteralA )
    {
    // That combination means we want to put the text into the assembly
    // language.  This is a compile-time operation, so the field has to be
    // a FldLiteralA.
    gg_insert_into_assemblerf( "%s %s",
                              ASM_COMMENT_START,
                              refs[n-1].field->data.original());
    }
  else
    {
    parser_display_internal(file_descriptor,
                            refs[n-1],
                            advance ? DISPLAY_ADVANCE : DISPLAY_NO_ADVANCE);
    }
  if( needs_closing )
    {
    gg_close(file_descriptor);
    }

  cursor_at_sol = advance;
  }

static
bool  // Returns false for literals; true for named variables
get_exhibit_name(tree file_descriptor, const cbl_refer_t &arg)
  {
  bool retval;
  if( is_literal(arg.field) )
    {
    // If something is a literal, we just display the literal value
    parser_display_internal(file_descriptor,
                            arg,
                            DISPLAY_NO_ADVANCE);
    retval = false;
    }
  else
    {
    // It's not a literal, so we show its name, and the names or literal
    // values) of any qualifier subscripts or refmods
    gg_write( file_descriptor,
              gg_string_literal(arg.field->name),
              build_int_cst_type(SIZE_T, strlen(arg.field->name)) );

    if( arg.subscripts.size() )
      {
      // This refer has subscripts:
      gg_write( file_descriptor,
                gg_string_literal("("),
                integer_one_node );
      for(size_t i=0; i<arg.subscripts.size(); i++)
        {
        if( i > 0 )
          {
          gg_write( file_descriptor,
                    gg_string_literal(","),
                    integer_one_node );
          }
        get_exhibit_name(file_descriptor, arg.subscripts[i]);
        }
      gg_write( file_descriptor,
                gg_string_literal(")"),
                integer_one_node );
      }
    if( arg.refmod.from || arg.refmod.len )
      {
      gg_write( file_descriptor,
                gg_string_literal("("),
                integer_one_node );
      if( arg.refmod.from )
        {
        get_exhibit_name(file_descriptor, *(arg.refmod.from));
        }
      gg_write( file_descriptor,
                gg_string_literal(":"),
                integer_one_node );
      if( arg.refmod.len )
        {
        get_exhibit_name(file_descriptor, *(arg.refmod.len));
        }
      gg_write( file_descriptor,
                gg_string_literal(")"),
                integer_one_node );
      }
    retval = true;
    }
  return retval;
  }

void
parser_exhibit( bool /*changed*/, bool /*named*/,
                const std::vector<cbl_refer_t> &args )
  {
  tree file_descriptor = gg_define_variable(INT);
  gg_assign(file_descriptor, integer_one_node);   // stdout is file descriptor 1.

  for(size_t i=0; i<args.size(); i++)
    {
    CHECK_FIELD(args[i].field);
    if(i > 0)
      {
      // When there more than one argument, the second through Nth get a space
      // in front of them.
      gg_write( file_descriptor,
                gg_string_literal(" "),
                integer_one_node);
      }
    if( get_exhibit_name(file_descriptor, args[i]) )
      {
      gg_write( file_descriptor,
                gg_string_literal("="),
                integer_one_node);
      parser_display_internal(file_descriptor,
                              args[i],
                              DISPLAY_NO_ADVANCE);
      }
    }
  gg_write( file_descriptor,
            gg_string_literal("\n"),
            integer_one_node);
  cursor_at_sol = true;
  }

void
parser_assign( size_t nC, cbl_num_result_t *C,
               struct cbl_refer_t sourceref,
               cbl_label_t *on_error,
               cbl_label_t *not_error,
               cbl_label_t *compute_error)
  {
  Analyze();
  RETURN_IF_PARSE_ONLY;
  // There might, or might not, already be error and/or not_error labels:
  set_up_on_exception_label(on_error);
  set_up_on_exception_label(not_error);
  set_up_compute_error_label(compute_error);

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    }

  TRACE1
    {
    TRACE1_HEADER
    char ach[32];
    sprintf(ach, HOST_SIZE_T_PRINT_DEC " target%s",
            (fmt_size_t)nC, nC==1 ? "" : "s");
    TRACE1_TEXT(ach);
    if( on_error )
      {
      TRACE1_TEXT("; with on_error");
      }
    if( not_error )
      {
      TRACE1_TEXT("; with not_error");
      }
    }

  tree error_flag = gg_define_variable(INT, 0L);

  for(size_t i=0; i<nC; i++ )
    {
    TRACE1
      {
      char ach[48];
      sprintf(ach, "Processing target number " HOST_SIZE_T_PRINT_DEC,
              (fmt_size_t)i);
      TRACE1_INDENT
      TRACE1_TEXT(ach);
      }
    cbl_refer_t& destref( C[i].refer );
    cbl_round_t rounded = C[i].rounded;
    SHOW_PARSE
      {
      if(i)
        {
        SHOW_PARSE_INDENT
        }
      if( sourceref.field && is_figconst_low(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" LOW-VALUE")
        }
      else if( sourceref.field && is_figconst_zero(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" ZERO-VALUE")
        }
      else if( sourceref.field && is_figconst_space(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" SPACE-VALUE")
        }
      else if( sourceref.field && is_figconst_quote(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" QUOTE-VALUE")
        }
      else if( sourceref.field && is_figconst_high(sourceref.field) )
        {
        SHOW_PARSE_TEXT(" HIGH-VALUE")
        }
      else
        {
        SHOW_PARSE_REF(" ", sourceref)
        }
      SHOW_PARSE_REF(" TO ", destref)
      switch(rounded)
        {
        case away_from_zero_e:
          SHOW_PARSE_TEXT(" AWAY_FROM_ZERO")
          break;
        case nearest_toward_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_TOWARD_ZERO")
          break;
        case toward_greater_e:
          SHOW_PARSE_TEXT(" TOWARD_GREATER")
          break;
        case toward_lesser_e:
          SHOW_PARSE_TEXT(" TOWARD_LESSER")
          break;
        case nearest_away_from_zero_e:
          SHOW_PARSE_TEXT(" NEAREST_AWAY_FROM_ZERO")
          break;
        case nearest_even_e:
          SHOW_PARSE_TEXT(" NEAREST_EVEN")
          break;
        case prohibited_e:
          SHOW_PARSE_TEXT(" PROHIBITED")
          break;
        case truncation_e:
          SHOW_PARSE_TEXT(" TRUNCATED")
          break;
        default:
          gcc_unreachable();
          break;
        }
      }

    CHECK_FIELD(destref.field);
    CHECK_FIELD(sourceref.field);

    tree erf = gg_define_variable(INT);
    if( on_error )
      {
      // There is an ON ERROR clause.  When there is an ON ERROR clause, and
      // there is an error, the TARGET values are to be left unchanged.
      IF(compute_error->structs.compute_error->compute_error_code,
         ne_op,
         integer_zero_node )
        {
        // There was an error, so we do NOT replace the destref with the
        // sourceref value
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("on_error clause; computional error occurred")
          }
        }
      ELSE
        {
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("on_error clause; no computational error")
          }
        // There was no computational error.  Call the move routine that does
        // not replace the target when there is a size error:
        TREEPLET tsource;
        treeplet_fill_source(tsource, sourceref);
        static bool check_for_error = true;
        move_helper(erf,
                    destref,
                    sourceref,
                    tsource,
                    rounded,
                    check_for_error,
                    true);
        gg_assign(error_flag, gg_bitwise_or(error_flag, erf));
        IF(error_flag, ne_op, integer_zero_node)
          {
          TRACE1
            {
            TRACE1_INDENT
            TRACE1_TEXT("on_error clause; a move error occurred")
            }
          // There was an error during the move.  Set the exception status
          // information:
          gg_call(  VOID,
                    "__gg__process_compute_error",
                    build_int_cst_type(INT, compute_error_truncate),
                    NULL_TREE);
          // But because there is an ON ERROR clause, suppress DECLARATIVE
          // processing
          gg_assign(var_decl_exception_code, integer_zero_node);
          }
        ELSE
          {
          TRACE1
            {
            TRACE1_INDENT
            TRACE1_TEXT("on_error clause; no move")
            }
          }
        ENDIF
        }
      ENDIF
      }
    else
      {
      // There is no ON_ERROR clause, so we do the truncation type move, but
      // with one exception.  If the error was an exponentiation error that
      // resulted in a NaN, we *don't* do the move:

      IF( gg_bitwise_and( compute_error->structs.compute_error->compute_error_code,
                          build_int_cst_type(INT,
                                             compute_error_exp_minus_by_frac
                                             | compute_error_divide_by_zero)),
                          ne_op,
                          integer_zero_node )
        {
        // It was a NaN, so don't do the move
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("Not moving the NaN")
          }
        }
      ELSE
        {
        TRACE1
          {
          TRACE1_INDENT
          TRACE1_TEXT("Doing the move")
          }
        TREEPLET tsource;
        treeplet_fill_source(tsource, sourceref);
        static bool check_for_error = true;
        move_helper(erf,
                    destref,
                    sourceref,
                    tsource,
                    rounded,
                    check_for_error,
                    false);
        gg_assign(error_flag, gg_bitwise_or(error_flag, erf));
        IF(error_flag, ne_op, integer_zero_node)
          {
          // There was an error during the move.  Set the exception status
          // information:
          TRACE1
            {
            TRACE1_INDENT
            TRACE1_TEXT("Error during the move; calling __gg__process_compute_error")
            }
          gg_call(  VOID,
                    "__gg__process_compute_error",
                    build_int_cst_type(INT, compute_error_truncate),
                    NULL_TREE);
          }
        ELSE
          {
          }
        ENDIF
        }
      ENDIF
      }
    TRACE1
      {
      TRACE1_INDENT
      TRACE1_FIELD("source ", sourceref.field, "")
      TRACE1_INDENT
      TRACE1_FIELD("dest ", destref.field, "")
      TRACE1_END
      }
    }

  if( on_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down on_error GOTO into")
      SHOW_PARSE_LABEL_OK(" ", on_error)
      }
    IF( gg_bitwise_or(error_flag,
                      compute_error->structs.compute_error->compute_error_code),
                      ne_op,
                      integer_zero_node )
      {
      gg_append_statement( on_error->structs.arith_error->into.go_to );
      }
    ELSE
      ENDIF
    }
  else
    {
    // We weren't given an explicit ON SIZE ERROR label, so we need to go
    // with the NO ERROR CLAUSE behavior
    if( compute_error )
      {
      gg_call(  VOID,
                "__gg__process_compute_error",
                compute_error->structs.compute_error->compute_error_code,
                NULL_TREE);
      }
    }

  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down not_error GOTO into")
      SHOW_PARSE_LABEL_OK(" ", not_error)
      }
    IF( compute_error->structs.compute_error->compute_error_code, eq_op, integer_zero_node )
      {
      gg_append_statement( not_error->structs.arith_error->into.go_to );
      }
    ELSE
      ENDIF
    }

  if( on_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down on_error LABEL BOTTOM:")
      SHOW_PARSE_LABEL_OK(" ", on_error)
      }
    gg_append_statement( on_error->structs.arith_error->bottom.label );
    }

  if( not_error )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(" Laying down not_error LABEL BOTTOM:")
      SHOW_PARSE_LABEL_OK(" ", not_error)
      }
    gg_append_statement( not_error->structs.arith_error->bottom.label );
    }

  SHOW_PARSE
    {
    SHOW_PARSE_END
    }
  }

void
parser_initialize_table(size_t nelem,
                        cbl_refer_t src,
                        size_t nspan,
                        const cbl_bytespan_t spans[],
                        size_t table, // symbol table index
                        size_t ntbl,
                        const cbl_subtable_t tbls[])
  {
  /*
   * "nelem" represents the number of elements in the table.
   * "src" is the already-initialized first element of the table
   * to be initialized.  If nspan == 0, copy the whole record because
   * the record either has no filler, or WITH FILLER was specified.
   * Otherwise, the spans array comprises a set of {offset,end+1} pairs
   * representing sequences of consecutive non-FILLER fields.
   *
   * "table" is the symbol table index for the table being initialized.
   * It may appear in a subsequent call as part of the (sub)tbls array,
   * if it is nested in a higher-level table.
   */

  if( mode_syntax_only() ) return;

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER("src:  ",  src, " ")
    TRACE1_END
    }
  typedef size_t span_t[2];
  static_assert(sizeof(spans[0]) == sizeof(span_t), "pair size wrong");
  tree tspans = gg_define_variable(SIZE_T_P);
  tree ttbls  = gg_define_variable(SIZE_T_P);

  gg_assign(tspans,
            build_array_of_size_t(2*nspan,
                                  reinterpret_cast<const size_t *>(spans)));
  gg_assign(ttbls,
            build_array_of_size_t(2*ntbl,
                                  reinterpret_cast<const size_t *>(tbls)));

  gg_call(VOID,
          "__gg__mirror_range",
          build_int_cst_type(SIZE_T, nelem),
          gg_get_address_of(src.field->var_decl_node),
          refer_offset(src),
          build_int_cst_type(SIZE_T, nspan),
          tspans,
          build_int_cst_type(SIZE_T, table),
          build_int_cst_type(SIZE_T, ntbl),
          ttbls,
          NULL_TREE);

  gg_free(tspans);
  gg_free(ttbls);
  }

static void
restore_local_variables()
  {
  gg_call(VOID,
          "__gg__pop_local_variables",
          NULL_TREE);
  gg_decrement(var_decl_unique_prog_id);
  }

static inline bool
is_valuable( cbl_field_type_t type ) {
  /*  The name of this routine is a play on words, in English.  It doesn't
      mean "Is worth a lot".  It means "Can be converted to a value." */
  switch ( type ) {
  case FldInvalid:
  case FldGroup:
  case FldAlphanumeric:
  case FldNumericEdited:
  case FldLiteralA:
  case FldClass:
  case FldConditional:
  case FldForward:
  case FldSwitch:
  case FldDisplay:
    return false;
  // These are variable types that have to be converted from their
  // COBOL form to a little-endian binary representation so that they
  // can be conveyed BY CONTENT/BY VALUE in a CALL or user-defined
  // function activation.
  case FldAlphaEdited:
  case FldNumericDisplay:
  case FldNumericBinary:
  case FldFloat:
  case FldPacked:
  case FldNumericBin5:
  case FldLiteralN:
  case FldIndex:
  case FldPointer:
    return true;
  }
  cbl_internal_error( "%s:%d: invalid %<symbol_type_t%> %d", __func__, __LINE__, type );
  return false;
}

void parser_sleep(const cbl_refer_t &seconds)
  {
  RETURN_IF_PARSE_ONLY;
  if( seconds.field )
    {
    gg_get_address_of(seconds.field->var_decl_node);
    //refer_offset(seconds);
    //refer_size_source(seconds);

    gg_call(VOID,
            "__gg__sleep",
            gg_get_address_of(seconds.field->var_decl_node),
            refer_offset(seconds),
            refer_size_source(seconds),
            NULL_TREE);
    }
  else
    {
    // This is a naked place-holding CONTINUE.  Generate some do-nothing
    // code that will stick some .LOC information into the assembly language,
    // so that GDB-COBOL can display the CONTINUE statement.
    insert_nop(104);
    }
  }

void
parser_exit_program()
  { // exits back to COBOL only, else continue
  static cbl_label_t this_program = {};
  static cbl_refer_t magic_refer(&this_program, false);
  parser_exit( magic_refer );
  }

/*
 * If RETURNING was specified, the field is provided as an argument, no lookup
 * necessary.  refer.field == NULL means exit(0) unless ec != ec_none_e.
 * If ec == ec_all_e, that indicates RAISING LAST EXCEPTION was used.
 */

static
void
program_end_stuff(const cbl_refer_t &refer,
                        ec_type_t    ec)
  {
  // Looking for hijack here puts the hijacked code just before the
  // exit sequence
#ifdef ENABLE_HIJACKING
  static bool just_once = true;
  // We need the just_once state because this routine can be called more than
  // once.  Usually the parser handles it, but we have a "just-in-case" call
  // in parser_end_program() that sometimes is necessary.
  if(just_once && strcmp(current_function->our_name, "hijack_h") == 0)
    {
    just_once = false;
    fprintf(stderr, "This is a HIJACK BEFORE EXIT scenario.\n");
    hijacker();
    }
#endif
  // This is the moral equivalent of a C "return xyz;".

  // There cannot be both a non-zero exit status and an exception condition.
  gcc_assert( !(ec != ec_none_e && refer.field != NULL) );

  gg_call(VOID,
          "__gg__pseudo_return_flush",
          NULL_TREE);

  cbl_field_t *returner = refer.field ? refer.field : current_function->returning;

  if( returner )
    {
    cbl_field_type_t field_type = returner->type;
    tree return_type = tree_type_from_field(returner);
    tree retval   = gg_define_variable(return_type);

    gg_assign(retval, gg_cast(return_type, integer_zero_node));

    if( is_valuable( field_type ) )
      {
      // The field being returned is numeric.
      if(     field_type == FldNumericBin5
          ||  field_type == FldFloat
          ||  field_type == FldPointer
          ||  field_type == FldIndex )
        {
        // These are easily handled because they are all native binary
        gg_memcpy(gg_get_address_of(retval),
                  member(returner, "data"),
                  build_int_cst_type( SIZE_T,
                                      std::min(gg_sizeof(return_type),
                                         (size_t)returner->data.capacity())));
        }
      else
        {
        // The field_type has a PICTURE string, so we need to convert from the
        // COBOL form to native binary:
        tree value;
        get_binary_value( value, returner, return_type);
        gg_memcpy(gg_get_address_of(retval),
                  gg_get_address_of(value),
                  build_int_cst_type(SIZE_T, gg_sizeof(return_type)));
        }
      restore_local_variables();
      gg_return(retval);
      }
    else
      {
      // The RETURNING type is a group or alphanumeric

      // The byte array to be returned is in returning, which is a local
      // variable on the stack.  We need to make a copy of it to avoid the
      // error of returning a pointer to data on the stack.

      tree array_type = build_array_type_nelts(UCHAR,
                                    returner->data.capacity());
      tree array     =  gg_define_variable(array_type, vs_static);
      gg_memcpy(gg_pointer_to_array(array),
                member(returner->var_decl_node, "data"),
                member(returner->var_decl_node, "capacity"));

      tree actual = gg_cast(COBOL_FUNCTION_RETURN_TYPE,
                            gg_pointer_to_array(array));

      restore_local_variables();
      gg_return(actual);
      }
    }
  else
    {
    // There is no explicit value.  This means, by default (according to IBM),
    // we return the value found in RETURN-CODE:
    tree value = gg_define_variable(COBOL_FUNCTION_RETURN_TYPE);
    if( !hijacked )
      {
      gg_assign(value,
                gg_cast(COBOL_FUNCTION_RETURN_TYPE,
                        current_function->var_decl_return));
      }
    else
      {
      gg_assign(value, gg_cast(COBOL_FUNCTION_RETURN_TYPE, integer_zero_node));
      }
    restore_local_variables();
    gg_return(gg_cast(COBOL_FUNCTION_RETURN_TYPE, value));
    }
  }

void
parser_exit( const cbl_refer_t& refer,
             ec_type_t ec )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(    gg_trans_unit.function_stack.size()
        && current_function->returning
        && !refer.field)
      {
      // ->returning works only if there is no refer.field
      SHOW_PARSE_FIELD(" RETURNING ", current_function->returning);
      }
    if( gg_trans_unit.function_stack.size() && refer.field )
      {
      SHOW_PARSE_FIELD(" WITH STATUS ", refer.field);
      }
    if( gg_trans_unit.function_stack.size() && refer.prog_func )
      {
      SHOW_PARSE_TEXT(" refer.prog_func is non-zero")
      }

    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  if( hijacked )
    {
    // We need just_once because parser_exit gets called an extra time at the
    // end of file, just in case. That should be tracked down and handled so
    // that it gets called only once.
    static bool just_once = true;
    if( just_once )
      {
      just_once = false;
      tree function_type =
                     TREE_TYPE(DECL_RESULT(current_function->function_decl));
      tree operand = gg_define_variable(function_type);
      gg_assign(operand, build_int_cst_type(function_type, 0));
      tree modify = build2(   MODIFY_EXPR,
                              function_type,
                              DECL_RESULT(current_function->function_decl),
                              gg_cast(function_type, operand));
      tree stmt = build1(RETURN_EXPR, void_type_node, modify);
      gg_append_statement(stmt);
      }

    return;
    }

  if( refer.prog_func )
    {
    // We are processing EXIT PROGRAM.  If main() called us, we need to do
    // nothing.  Otherwise, this is a return
    IF( current_function->called_by_main_counter, eq_op, integer_zero_node )
      {
      // This function wasn't called by main, so we treat it like a GOBACK
      program_end_stuff(refer, ec);
      }
    ELSE
      {
      // This function was called by main.  Is it the first call, or is it
      // recursive?
      IF( current_function->called_by_main_counter, gt_op, integer_one_node )
        {
        // This was a recursive call into the function originally called by
        // main.  Because we are under the control of a calling program, we
        // treat this like a GOBACK
        program_end_stuff(refer, ec);
        }
      ELSE
        {
        // We are not under the control of a calling program, meaning that we
        // were called by main().  So, we do nothing, meaning we behave like
        // a CONTINUE.
        }
        ENDIF
      }
      ENDIF
    }
  else
    {
    IF( current_function->called_by_main_counter, gt_op, integer_zero_node )
      {
      // This wasn't an EXIT PROGRAM.  But in the case where we are the program
      // that was called by main(), we need to do some bookkeeping so that we
      // respond properly to an EXIT PROGRAM should one appear
      gg_decrement(current_function->called_by_main_counter);
      }
    ELSE
      {
      }
      ENDIF
    program_end_stuff(refer, ec);
    }
  }

static void
walk_initialization(cbl_field_t *field, bool initialized, bool deallocate)
  {
  if( !(field->attr & based_e) )
    {
    // We are concerned only with BASED variables
    return;
    }
  symbol_elem_t *e = symbol_at(field_index(field));
  bool first_time = true;
  while( e < symbols_end() )
    {
    symbol_elem_t& element = *e++;
    if( element.type == SymField )
      {
      cbl_field_t *this_one = cbl_field_of(&element);
      if( !first_time )
        {
        if( this_one->level == LEVEL01 || this_one->level == LEVEL77 )
          {
          // Having encountered the next 01 or 77, we are done
          break;
          }
        }
      first_time = false;

      // We need to propagate the based_e attribute:
      cbl_field_of(&element)->attr |= based_e;

      if( this_one->level == 00 )
        {
        // Ignore LEVEL00 "INDEXED BY" variables
        continue;
        }
      if(deallocate)
        {
        gg_assign(member(this_one->var_decl_node, "data"),
                  gg_cast(UCHAR_P, null_pointer_node));
        }
      else
        {
        gg_assign(member(this_one->var_decl_node, "data"),
                  gg_add(member(field->var_decl_node, "data"),
                          build_int_cst_type(SIZE_T, this_one->offset)));
        if(     this_one->level == 66
            ||  this_one->level == 88
            ||  symbol_redefines(this_one) )
          {
          continue;
          }
        if( !initialized )
          {
          // This is ALLOCATE Rule 9) in ISO 2023
          if( this_one->type == FldPointer )
            {
            gg_memset(member(this_one->var_decl_node, "data"),
                   integer_zero_node,
                   build_int_cst_type(SIZE_T, this_one->data.capacity()));
            }
          }
        }
      }
    }
  }

void
parser_allocate(cbl_refer_t size_or_based,
                cbl_refer_t returning,
                bool initialized )
  {
  /*
   * If the 1st parameter has based_e attribute, the field it is based on defines
   * the number of bytes to allocate. In that case, "returning" is optional and
   * may have a NULL field.  Otherwise the 1st parameter is a numeric value and
   * allocated space is assigned to "returning", which is of type FldPointer.
   */

  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_REF(" size_or_based from:", size_or_based)
    SHOW_PARSE_INDENT
    SHOW_PARSE_REF("returning:         ", returning)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER("size_or_based: ", size_or_based, "");
    TRACE1_INDENT
    TRACE1_REFER("returning:     ", size_or_based, "");
    TRACE1_END
    }

  if( returning.field )
    {
    // If there is a returning, it has to be a pointer
    gcc_assert(returning.field->type == FldPointer);
    }

  if( !(size_or_based.field->attr & based_e) )
    {
    // If the first is not based, then there must be a returning
    gcc_assert(returning.field);
    }

  cbl_field_t *f_working = current_options().initial_working();
  cbl_field_t *f_local   = current_options().initial_local();

  unsigned int default_byte = wsclear() ? *wsclear() : (uint32_t)(-1);

  gg_call(VOID,
          "__gg__allocate",
          gg_get_address_of(size_or_based.field->var_decl_node),
          refer_offset(size_or_based) ,
          initialized ? integer_one_node : integer_zero_node,
          build_int_cst_type(INT, default_byte),
          f_working ? gg_get_address_of(f_working->var_decl_node) : null_pointer_node,
          f_local   ? gg_get_address_of(f_local->  var_decl_node) : null_pointer_node,
          returning.field ? gg_get_address_of(returning.field->var_decl_node)
                          : null_pointer_node,
          returning.field ? refer_offset(returning)
                          : size_t_zero_node,
          NULL_TREE);
  walk_initialization(size_or_based.field, initialized, false);
  }

void
parser_free( size_t n, cbl_refer_t refers[] )
  {
  if( mode_syntax_only() ) return; // Normally handled by SHOW_PARSE, if present

  Analyze();
  for( auto p = refers; p < refers + n; p++ )
    {
    gcc_assert( ! p->all );
    gcc_assert( ! p->is_refmod_reference() );
    if( !(p->field->type == FldPointer || p->addr_of || (p->field->attr & based_e)) )
      {
      dbgmsg("Deallocating %s means it has to be FldPointer or addr_of or based_e",
             p->field->name);
      }
    gcc_assert( p->field->type == FldPointer || p->addr_of || (p->field->attr & based_e) );

    gg_call(VOID,
            "__gg__deallocate",
            gg_get_address_of(p->field->var_decl_node),
            refer_offset(*p),
            p->addr_of ? integer_one_node : integer_zero_node,
            NULL_TREE);
    walk_initialization(p->field, false, true);
    }
  }

static
cbl_label_addresses_t *
label_fetch(struct cbl_label_t *label)
  {
  if( !label->structs.goto_trees )
    {
    label->structs.goto_trees
      = static_cast<cbl_label_addresses_t *>
        (xmalloc(sizeof(struct cbl_label_addresses_t)));
    gcc_assert(label->structs.goto_trees);

    gg_create_goto_pair(&label->structs.goto_trees->go_to,
                        &label->structs.goto_trees->label);
    }
  return label->structs.goto_trees;
  }

// This routine cloned from parse_ante.h
static inline cbl_field_t *
register_find( const char *name ) {
  size_t iprog = current_program_index();
  auto found = symbol_find( iprog, std::list<const char*>(1, name) );
  gcc_assert(found.second);
  return cbl_field_of(found.first);
}

void
parser_xml_parse( cbl_label_t *instance,
                  cbl_refer_t input,
                  cbl_field_t *encoding,
                  cbl_field_t *validating,
                  bool returns_national,
                  cbl_label_t *from_proc,
                  cbl_label_t *to_proc )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL_OK("", instance)
    SHOW_PARSE_REF(" ", input)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  // We know that this routine comes first in the sequence, so we can
  // create the goto/label pairs here:

  instance->structs.xml_parse = static_cast<struct cbl_xml_parse_t *>
                                  (xmalloc(sizeof(struct cbl_xml_parse_t)));
  gcc_assert(instance->structs.xml_parse);

  gg_create_goto_pair(&instance->structs.xml_parse->over.go_to,
                      &instance->structs.xml_parse->over.label);
  gg_create_goto_pair(&instance->structs.xml_parse->exception.go_to,
                      &instance->structs.xml_parse->exception.label);
  gg_create_goto_pair(&instance->structs.xml_parse->no_exception.go_to,
                      &instance->structs.xml_parse->no_exception.label);

  // We need to create a COBOL ENTRY point into this function.  That entry
  // point will be used by __gg__xml_parse to perform from_proc through to_proc
  // as part of processing the libxml2 callbacks.

  char ach[64];
  static int instance_counter = 1;
  sprintf(ach,
          "_%s_xml_callback_%d",
          current_function->our_name,
          instance_counter++);

  cbl_field_data_t data( 0, strlen(ach), 0,0, ach );
  cbl_field_t for_entry(FldAlphanumeric, 0, data, 0);
  for_entry.codeset.set(iconv_CP1252_e);

  // build an island for the callback:
  tree island_goto;
  tree island_label;
  gg_create_goto_pair(&island_goto,
                      &island_label);

  gg_append_statement(island_goto);
  // This creates the separate _xml_callback function
  parser_entry(&for_entry, 0, nullptr);
  // When invoked, the callback performs the processing procedures
  parser_perform(from_proc, to_proc);
  // And then returns back to the caller
  gg_return(0);
  gg_append_statement(island_label);

  // We need the three xml special registers:
  cbl_field_t *xml_event = register_find("XML-EVENT");
  cbl_field_t *xml_code  = register_find("XML-CODE");
  cbl_field_t *xml_text  = register_find("XML-TEXT");

  // With the callback in place, we are ready to call the library:
  tree pcallback = gg_get_function_address(VOID, ach);

  tree erc = gg_define_variable(INT);
  gg_assign(erc, gg_call_expr(INT,
                              "__gg__xml_parse",
                              gg_get_address_of(input.field->var_decl_node),
                              refer_offset(input),
                              refer_size_source(input),
                              encoding ?
                                  gg_get_address_of(encoding->var_decl_node)
                                : null_pointer_node,
                              validating ?
                                  gg_get_address_of(validating->var_decl_node)
                                : null_pointer_node,
                              build_int_cst_type(INT, returns_national),
                              pcallback,
                              gg_get_address_of(xml_event->var_decl_node),
                              gg_get_address_of(xml_code ->var_decl_node),
                              gg_get_address_of(xml_text ->var_decl_node),
                              NULL_TREE));
  IF( erc, ne_op, integer_zero_node )
    {
    //gg_printf("__gg__xml_parse() failed with erc %d\n", erc, NULL_TREE);
    gg_append_statement(instance->structs.xml_parse->exception.go_to);
    }
  ELSE
    {
    //gg_printf("__gg__xml_parse() apparently succeeded\n", NULL_TREE);
    gg_append_statement(instance->structs.xml_parse->no_exception.go_to);
    }
  ENDIF
  }

void
parser_xml_on_exception( cbl_label_t *instance )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL_OK(" ", instance)
    SHOW_PARSE_END
    }
  gg_append_statement(instance->structs.xml_parse->over.go_to);
  gg_append_statement(instance->structs.xml_parse->exception.label);
  }

void
parser_xml_not_exception( cbl_label_t *instance )
{
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL_OK(" ", instance)
    SHOW_PARSE_END
    }
  gg_append_statement(instance->structs.xml_parse->over.go_to);
  gg_append_statement(instance->structs.xml_parse->no_exception.label);
  }

void parser_xml_end( cbl_label_t *instance )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL_OK(" ", instance)
    SHOW_PARSE_END
    }
  gg_append_statement(instance->structs.xml_parse->over.label);
  }

void
parser_arith_error(cbl_label_t *arithmetic_label)
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We are entering either SIZE ERROR or NOT SIZE ERROR code
  RETURN_IF_PARSE_ONLY;

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO OVER")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT(" Laying down LABEL INTO:")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_END
    }

  CHECK_LABEL(arithmetic_label);

  set_up_on_exception_label(arithmetic_label);

  // Jump over the [NOT] ON EXCEPTION code that is about to be laid down
  gg_append_statement( arithmetic_label->structs.arith_error->over.go_to );
  // Create the label that allows the following code to be executed at
  // when an ERROR, or NOT ERROR, has been determined to have taken place:
  gg_append_statement( arithmetic_label->structs.arith_error->into.label );
  }

void
parser_arith_error_end(cbl_label_t *arithmetic_label)
  {
  // We can't use Analyze() on this one, because the exit ends up being laid
  // down before the enter when the goto logic gets untangled by the compiler.

  // We have reached the end of the ERROR, or NOT ERROR, code.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Laying down GOTO BOTTOM")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT(" Laying down LABEL OVER:")
    SHOW_PARSE_LABEL(" ", arithmetic_label)
    SHOW_PARSE_END
    }

  CHECK_LABEL(arithmetic_label);

  // Jump to the end of the arithmetic code:
  gg_append_statement( arithmetic_label->structs.arith_error->bottom.go_to );
  // Lay down the label that allows the ERROR/NOT ERROR instructions
  // to exist in a lacuna that doesn't get executed unless somebody jumps
  // to it:
  gg_append_statement( arithmetic_label->structs.arith_error->over.label );
  }

static void
propogate_linkage_offsets(cbl_field_t *field, tree base)
  {
  if( field->level == LEVEL01 || field->level == LEVEL77 )
    {
    field->data_decl_node = base;
    symbol_elem_t *e = symbol_at(field_index(field));
    // We already updated the data pointer of the first element:
    e += 1;
    while( e < symbols_end() )
      {
      symbol_elem_t& element = *e++;
      if( element.type == SymField )
        {
        cbl_field_t *this_one = cbl_field_of(&element);
        if( this_one->level == LEVEL01 || this_one->level == LEVEL77 )
          {
          // We have encountered another level 01/77.  If this LEVEL 01 had a
          // parent, then we have to assume that this is a redefines of another
          // level 01/77.
          if( this_one->parent )
            {
            // And, gloriously and frighteningly, it can be handled by
            // recursion:
            propogate_linkage_offsets(this_one, base);
            }
          else
            {
            // Having encountered the next 01 or 77, we are done
            break;
            }
          }
        if( this_one->level == 00 )
          {
          // Ignore LEVEL00 "INDEXED BY" variables
          continue;
          }
        tree offset = gg_define_variable(SIZE_T);
        IF( base, eq_op, gg_cast(UCHAR_P, null_pointer_node) )
          {
          gg_assign(offset, size_t_zero_node);
          }
        ELSE
          {
          gg_assign(offset, member(this_one, "offset"));
          }
        ENDIF
        this_one->data_decl_node = base;
        member( this_one,
                "data",
                gg_add(base, offset));
        }
      }
    }
  }

static bool initialized_data = false;
static void
initialize_the_data()
  {
  RETURN_WHEN_HIJACKED;

  if( initialized_data )
    {
    return;
    }
  initialized_data = true;
  // Here is where we initialize the run-time list of currency symbols:
  const char *default_currency = "$";

  // This is one-time initialization of the libgcobol program state stack
  gg_call(VOID,
          "__gg__init_program_state",
          build_int_cst_type(INT, current_encoding(display_encoding_e)),
          build_int_cst_type(INT, current_encoding(national_encoding_e)),
          NULL_TREE);

  // We initialize currency both at compile time and run time
  __gg__currency_sign_init();
  gg_call(VOID,
          "__gg__currency_sign_init",
          NULL_TREE);

  gg_call(VOID,
          "__gg__set_program_name",
          gg_string_literal( current_filename.back().c_str() ),
          NULL_TREE);

  for(int symbol=0; symbol<256; symbol++)
    {
    const char *sign = symbol_currency(symbol);
    if( sign )
      {
      default_currency = NULL;

      // Both compile-time and run-time
      __gg__currency_sign(symbol, sign);
      gg_call(VOID,
              "__gg__currency_sign",
              build_int_cst_type(INT, symbol),
              build_string_literal(strlen(sign)+1, sign),
              NULL_TREE);
      }
    }
  if( default_currency )
    {
    __gg__currency_sign(default_currency[0], default_currency);
    gg_call(VOID,
            "__gg__currency_sign",
            char_nodes[(int)default_currency[0]],
            gg_string_literal(default_currency),
            NULL_TREE);
    }

  // It's time to tell the library about DECIMAL-POINT IS COMMA:
  if( symbol_decimal_point() == ',' )
    {
    __gg__decimal_point     = ascii_comma ;
    __gg__decimal_separator = ascii_period ;
    gg_call(VOID,
            "__gg__decimal_point_is_comma",
            NULL_TREE);
    }

  // This is where we tell the library about this program's initialization
  // values:
  cbl_field_t *init_working = current_options().initial_working();
  cbl_field_t *init_local   = current_options().initial_local();
  gg_call(VOID,
          "__gg__initialization_values",
          build_int_cst_type(UINT, wsclear() ? *wsclear()
                                    : static_cast<uint32_t>(NOT_A_CHARACTER)),
          init_working ? gg_get_address_of(init_working->var_decl_node)
                       : null_pointer_node,
          init_local   ? gg_get_address_of(init_local->var_decl_node)
                       : null_pointer_node,
          NULL_TREE);
  }

static
void
establish_using(size_t nusing,
                cbl_ffi_arg_t args[] )
  {
  if( nusing )
    {
    for(size_t i=0; i<nusing; i++)
      {
      // This code is relevant at compile time.  It takes each
      // expected formal parameter and tacks it onto the end of the
      // function's arguments chain.
      char *ach = xasprintf("_p_%s", args[i].refer.field->name);
      tree par_type;
      if( args[i].crv == by_value_e )
        {
        par_type = tree_type_from_refer(args[i].refer);
        }
      else
        {
        par_type = VOID_P;
        }
      chain_parameter_to_function(current_function->function_decl, par_type, ach);
      free(ach);
      }

    // During the call, we saved the parameter_count and an array of variable
    // lengths.  We need to look at those values if, and only if, one or more
    // of our USING arguments has an OPTIONAL flag or if one of our targets is
    // marked as VARYING.
    bool check_for_parameter_count = false;
    for(size_t i=0; i<nusing; i++)
      {
      if( args[i].optional )
        {
        check_for_parameter_count = true;
        break;
        }
      if( args[i].refer.field->attr & any_length_e )
        {
        check_for_parameter_count = true;
        break;
        }
      }

    if( check_for_parameter_count )
      {
      IF( var_decl_call_parameter_signature,
          eq_op,
          gg_cast(CHAR_P, current_function->function_address) )
        {
        // We know to use var_decl_call_parameter_count, so unflag this
        // pointer to avoid problems in the ridiculous possibility of
        // COBOL-A calls C_B calls COBOL_A
        gg_assign(var_decl_call_parameter_signature,
                  gg_cast(CHAR_P, null_pointer_node));
        }
      ELSE
        {
        // We were apparently called by a C routine, not a COBOL routine, so
        // make sure we don't get shortchanged by a count left behind from an
        // earlier COBOL call.
        gg_assign(var_decl_call_parameter_count,
                  build_int_cst_type(INT, A_ZILLION));
        }
      ENDIF
      }
    else
      {
      // None of our parameters require a count, so make sure we don't get
      // bamboozled by a count left behind from an earlier COBOL call.
      gg_assign(var_decl_call_parameter_count,
                build_int_cst_type(INT, A_ZILLION));
      }

    // There are 'nusing' elements in the PROCEDURE DIVISION USING list.

    tree parameter = NULL_TREE;
    tree rt_i = gg_define_variable(INT);
    for(size_t i=0; i<nusing; i++)
      {
      // And this generates run-time execution code. The
      // generated code picks up, at run time, the variable we just
      // established in the chain at compile time.

      // It makes more sense if you don't think about it too hard.

      // Arriving here means that we are processing an instruction like
      // this:
      // PROCEDURE DIVISION USING using[0] using[1] ... using using[nusing-1]

      // When __gg__call_parameter_count is equal to A_ZILLION, then this is
      // an OTHER-TO-COBOL call and the var_decl_call_parameter_lengths array
      // is not valid

      // Sort out the USING BY; it can be BY REFERENCE or BY VALUE:
      cbl_ffi_crv_t crv = args[i].crv;
      cbl_field_t *new_var = args[i].refer.field;

      if( crv == by_value_e )
        {
        switch(new_var->type)
          {
          case FldGroup:
          case FldAlphanumeric:
          case FldAlphaEdited:
          case FldNumericEdited:
            crv = by_reference_e;
            break;
          default:
            break;
          }
        }

      // We need to be able to restore prior arguments when doing recursive
      // calls:
      IF( member(args[i].refer.field->var_decl_node, "data"),
          ne_op,
          gg_cast(UCHAR_P, null_pointer_node) )
        {
        gg_call(VOID,
                "__gg__push_local_variable",
                gg_get_address_of(args[i].refer.field->var_decl_node),
                NULL_TREE);
        }
      ELSE
        ENDIF

      if( crv == by_reference_e )
        {
        // The passed parameter, if it exists, is a pointer to a COBOL
        // variable's data area
        tree reference = gg_define_variable(UCHAR_P);
        gg_assign(rt_i, build_int_cst_type(INT, i));
        IF( rt_i, lt_op , var_decl_call_parameter_count )
          {
          if( i == 0 )
            {
            // This is the first parameter.
            parameter = DECL_ARGUMENTS(current_function->function_decl);
            }
          else
            {
            // These are subsequent parameters
            parameter = TREE_CHAIN(parameter);
            }
          gg_assign(reference, gg_cast(UCHAR_P, parameter));

          if( args[i].refer.field->attr & any_length_e )
            {
            // gg_printf("side channel: Length of \"%s\" is %ld\n",
                      // member(args[i].refer.field->var_decl_node, "name"),
                      // gg_array_value(var_decl_call_parameter_lengths, rt_i),
                      // NULL_TREE);

            // Get the length from the global lengths[] side channel.
            gg_assign(member(args[i].refer.field->var_decl_node, "capacity"),
                      gg_array_value(var_decl_call_parameter_lengths, rt_i));
            }
          }
        ELSE
          {
          gg_assign(reference, gg_cast(UCHAR_P, null_pointer_node));
          }
        ENDIF
        if(     cobol_target_big_endian()  // cppcheck-suppress knownConditionTrueFalse
            &&  (    args[i].refer.field->type == FldNumericBin5
                  || args[i].refer.field->type == FldNumericBinary) )
          {
          // We have another thing to think about.  The reference we are
          // processing might have come from an intermediate, and those are
          // sixteen-byte values.  On a little-endian machine we can just use
          // the value as-is, because the extra zeroes are to the right of the
          // one we need.  But a big endian sixteen-byte value has a bunch of
          // leading zeroes, and we need to skip past them.
          tree offset = gg_define_variable(SIZE_T);
          // Pick up the length metadata.
          gg_assign(offset,
                    gg_array_value(var_decl_call_parameter_lengths, rt_i));

          // That value is probably sixteen.  Subtract the length of our target
          // value from that.
          gg_assign(offset,
                    gg_subtract(offset,
                                member(args[i].refer.field->var_decl_node,
                                       "capacity")));
          // And add that value to 'reference'
          gg_assign(reference, gg_add(reference, offset));
          // This code was added when I encountered a function call using
          // func( N - 1 ), where the function expected a single-byte value.
          // In little-endian, no problem, because the calculated value of 3
          // produced a sixteen-byte 0x03 00 00 00.....  But in big-endian,
          // the sixteen bytes are 0x00 00 00 00 ... 00 03.  The above
          // calculation starts with sixteen, subtracts one from it to get
          // fifteen, and then adds fifteen to 'reference' to point to the
          // 0x03.
          }

        // 'parameter' is a reference, so it it becomes the data member of
        // the cblc_field_t COBOL variable.
        gg_assign(member(args[i].field()->var_decl_node, "data"), reference);

        // We need to apply reference + offset to the LINKAGE variable
        // and all of its children
        propogate_linkage_offsets( args[i].field(), reference );
        }

      if( crv == by_value_e )
        {
        tree value_type = tree_type_from_field(new_var);

        // 'parameter' is the 64-bit or 128-bit value that was placed on the stack
        tree value = gg_define_variable(value_type);

        gg_assign(rt_i, build_int_cst_type(INT, i));
        IF( rt_i, lt_op , var_decl_call_parameter_count )
          {
          if( i == 0 )
            {
            // This is the first parameter.
            parameter = DECL_ARGUMENTS(current_function->function_decl);
            }
          else
            {
            // These are subsequent parameters
            parameter = TREE_CHAIN(parameter);
            }
          gg_memcpy(gg_get_address_of(value),
                    gg_get_address_of(parameter),
                    build_int_cst_type(SIZE_T, gg_sizeof(value)));

          if( args[i].refer.field->attr & any_length_e )
            {
            // gg_printf("side channel: Length of \"%s\" is %ld\n",
                      // member(args[i].refer.field->var_decl_node, "name"),
                      // gg_array_value(var_decl_call_parameter_lengths, rt_i),
                      // NULL_TREE);

            // Get the length from the global lengths[] side channel.  Don't
            // forget to use the length mask on the table value.
            gg_assign(member(args[i].refer.field->var_decl_node, "capacity"),
                      gg_array_value(var_decl_call_parameter_lengths, rt_i));
            }
          }
        ELSE
          {
          gg_assign(value, gg_cast(value_type, integer_zero_node));
          }
        ENDIF

        // Because new_var is linkage, at this point it has no data area. We
        // need to create that data area.
        tree array_type = build_array_type_nelts(UCHAR, new_var->data.capacity());
        tree data_decl_node = gg_define_variable( array_type,
                                                  NULL,
                                                  vs_static);
        gg_assign( member(new_var->var_decl_node, "data"),
                          gg_pointer_to_array(data_decl_node) );

        // And then put 'value' into place:
        if( new_var->type == FldFloat )
          {
          gg_memcpy(member(new_var->var_decl_node, "data"),
                    gg_get_address(value),
                    build_int_cst_type(SIZE_T, new_var->data.capacity()));
          }
        else
          {
          gg_call(VOID,
                  "__gg__assign_value_from_stack",
                  gg_get_address_of(new_var->var_decl_node),
                  gg_cast(INT128, value),
                  NULL_TREE);
          }
        // We now have to handle an oddball situation.  It's possible we are
        // dealing with
        //
        //    linkage section.
        //    01 var1
        //    01 var2 redefines var1
        //
        // If so, we have to give var2::data_pointer the same value as
        // var1::data_pointer
        //
        size_t our_index = symbol_index(symbol_elem_of(new_var));
        size_t next_index  = our_index + 1;
        // Look ahead in the symbol table for the next LEVEL01/77
        for(;;)
          {
          symbol_elem_t *e = symbol_at(next_index);
          if( e->type != SymField )
            {
            break;
            }
          cbl_field_t *next_var = cbl_field_of(e);
          if( !next_var )
            {
            break;
            }
          if( next_var->level == LEVEL01 || next_var->level == LEVEL77 )
            {
            if( next_var->parent == our_index )
              {
              gg_assign(member(next_var->var_decl_node, "data"),
                        member(new_var->var_decl_node, "data"));
              }
            break;
            }
          next_index += 1;
          }
        }
      }
    }
  }

void
parser_division(cbl_division_t division,
                cbl_field_t *returning,
                size_t nusing,
                cbl_ffi_arg_t args[] )
  {
  // This is called when the parser enters a COBOL program DIVISION.  See
  // parser_divide for the arithmetic operation.

  if( mode_syntax_only() ) return;

  // Do this before the SHOW_PARSE; it makes a little more sense when reviewing
  // the SHOW_PARSE output.
  if( division == identification_div_e )
    {
    initialized_data = false;
    if( gg_trans_unit.function_stack.size() >= 1 )
      {
      // This is a nested program.  So, we need to tie off the current
      // section:
      leave_paragraph_internal();
      leave_section_internal();
      }
    }

  if( division == environment_div_e )
    {
    initialized_data = false;
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    switch(division)
      {
      case identification_div_e:
        SHOW_PARSE_TEXT("IDENTIFICATION")
        break;
      case environment_div_e:
        SHOW_PARSE_TEXT("ENVIRONMENT")
        break;
      case data_div_e:
        SHOW_PARSE_TEXT("DATA")
        break;
      case procedure_div_e:
        SHOW_PARSE_TEXT("PROCEDURE")
        break;
      }

    SHOW_PARSE_END
    }

  if( division == data_div_e )
    {
    Analyze();
    initialize_the_data();
    }
  if( division == environment_div_e )
    {
    Analyze();
    }
  else if( division == procedure_div_e )
    {
    Analyze();

    RETURN_WHEN_HIJACKED;

    // Do some symbol table index bookkeeping.  current_program_index() is
    // valid at this point in time:
    current_function->our_symbol_table_index = current_program_index();
    const cbl_label_t *prog = cbl_label_of(symbol_at(current_program_index()));
    current_function->has_initial   = prog->initial;
    current_function->has_recursive = prog->recursive;

    // We have some housekeeping to do to keep track of the list of functions
    // accessible by us.

    // For every procedure, we need a variable that points to the list of
    // available program names.

    // We need a pointer to the array of program names
    char ach[2*sizeof(cbl_name_t)];
    if( !current_function->initialized )
      {
      // Do some symbol table index bookkeeping.  current_program_index() is valid
      // at this point in time:
      current_function->our_symbol_table_index = current_program_index();

      gg_create_goto_pair(&current_function->entry_switch_goto,
                          &current_function->entry_switch_label);

      // We have some housekeeping to do to keep track of the list of functions
      // accessible by us:

      // For every procedure, we need a variable that points to the list of
      // available program names.

      // We need a pointer to the array of program names
      sprintf(ach,
              "..accessible_program_list_" HOST_SIZE_T_PRINT_DEC,
              (fmt_size_t)current_function->our_symbol_table_index);
      tree prog_list = gg_define_variable(build_pointer_type(CHAR_P),
                                          ach, vs_file_static);

      // Likewise, we need a pointer to the array of pointers to functions:
      tree function_type =
        build_varargs_function_type_array( SIZE_T,
                                           0,     // No parameters yet
                                           NULL); // And, hence, no types
      tree pointer_type = build_pointer_type(function_type);
      tree constructed_array_type = build_array_type_nelts(pointer_type, 1);
      sprintf(ach,
              "..accessible_program_pointers_" HOST_SIZE_T_PRINT_DEC,
              (fmt_size_t)current_function->our_symbol_table_index);
      tree prog_pointers = gg_define_variable(
                                      build_pointer_type(constructed_array_type),
                                      ach,
                                      vs_file_static);
      gg_call(VOID,
              "__gg__set_program_list",
              build_int_cst_type(INT, current_function->our_symbol_table_index),
              gg_get_address_of(prog_list),
              gg_get_address_of(prog_pointers),
              NULL_TREE);

      if( gg_trans_unit.function_stack.size() == 1 )
        {
        gg_create_goto_pair(&label_list_out_goto,
                            &label_list_out_label);
        gg_create_goto_pair(&label_list_back_goto,
                            &label_list_back_label);
        gg_append_statement(label_list_out_goto);
        gg_append_statement(label_list_back_label);
        }

      tree globals_are_initialized = gg_declare_variable( INT,
                                                          "__gg__globals_are_initialized",
                                                          NULL,
                                                          vs_extern);
      IF( globals_are_initialized, eq_op, integer_zero_node )
        {
        // one-time initialization happens here

        // We need to establish the initial value of the UPSI-1 switch register
        // We are using IBM's conventions:
        // https://www.ibm.com/docs/en/zvse/6.2?topic=SSB27H_6.2.0/fa2sf_communicate_appl_progs_via_job_control.html
        // UPSI 10000110 means that bits 0, 5, and 6 are on, which means that
        // SW-0, SW-5, and SW-6 are on.
        gg_call(VOID,
                "__gg__onetime_initialization",
                NULL_TREE);

        // And then flag one-time initialization as having been done.
        gg_assign(globals_are_initialized, integer_one_node);

        // Let the library know what -dialect entries are in force:
        gg_assign(var_decl_dialects, build_int_cst_type(INT, cbl_dialects));
        }
      ELSE
        ENDIF
      }

    gg_append_statement(current_function->skip_init_label);
    // This is where we check to see if somebody tried to cancel us
    tree cancelled = gg_define_variable(INT);
    gg_assign(cancelled,
              gg_call_expr( INT,
                            "__gg__is_canceled",
                            current_function->function_address,
                            NULL_TREE));
    IF( cancelled, ne_op, integer_zero_node )
      {
      // Somebody flagged us for CANCEL, which means reinitialization, so we
      // need to find the _INITIALIZE_PROGRAM section label.

      // gg_printf("Somebody wants to cancel %s\n",
                // gg_string_literal(current_function->our_unmangled_name),
                // NULL_TREE);
      size_t initializer_index = prog->initial_section;
      cbl_label_t *initializer = cbl_label_of(symbol_at(initializer_index));
      parser_perform(initializer, true);  // true means suppress nexting
      }
    ELSE
      ENDIF

    // RETURNING variables are supposed to be in the linkage section, which
    // means that we didn't assign any storage to them during
    // parser_symbol_add().  We do that here.

    // returning also needs to behave like local storage, even though it is
    // in linkage.

    // This counter is used to help keep track of local variables
    gg_increment(var_decl_unique_prog_id);
    if( returning )
      {
      parser_local_add(returning);
      current_function->returning = returning;

      size_t nbytes = 0;
      tree returning_type = tree_type_from_field_type(returning, nbytes);
      gg_modify_function_type(current_function->function_decl, returning_type);
      }

    // Stash the returning variables for use during parser_return()
    current_function->returning = returning;

    cbl_field_t *return_code = cbl_field_of(symbol_at(return_code_register()));
    current_function->var_decl_return =
            gg_indirect(gg_cast(SHORT_P,
                                member(return_code->var_decl_node, "data")));

    if( gg_trans_unit.function_stack.size() == 1 )
      {
      // We are entering a new top-level program.

      if( current_function->has_initial || current_function->has_recursive )
        {
        // According to the IBM COBOL Language Specification, there is a list
        // of special registers that get cleared to zero or spaces when a
        // program has the INITIAL or RECURSIVE attribute.
        gg_assign(current_function->var_decl_return,
                  build_int_cst_type(SHORT, 0));
        }
      }

    // The parameters passed to this program might be 64 bits or 128 bits in
    // length.  We establish those lengths based on the types of the target
    // for each USING.

    gg_call(VOID,
            "__gg__pseudo_return_bookmark",
          NULL_TREE);

    // The MODULE-NAME function requires a stack of program names. We push the
    // name on here.  The first character is a 'T' or an 'N', where 'N' means
    // this is a nested program.

    if( gg_trans_unit.function_stack.size() > 1 )
      {
      // This is a nested program
      strcpy(ach, "N");
      }
    else
      {
      // This is a top-level program:
      strcpy(ach, "T");
      }
    strcat(ach, current_function->our_unmangled_name);
    gg_call(VOID,
            "__gg__module_name_push",
            gg_string_literal(ach),
            NULL_TREE);

    IF( var_decl_main_called, ne_op, integer_zero_node )
      {
      // We were just called by main:
      gg_assign(var_decl_main_called, integer_zero_node);
      gg_assign(current_function->called_by_main_counter, integer_one_node);
      }
    ELSE
      {
      // This isn't a call from main(), but it might be a recursive call to the
      // function that was called by main:
      IF(current_function->called_by_main_counter, ne_op, integer_zero_node)
        {
        // In that case, we bump the counter to keep track of things.
        gg_increment(current_function->called_by_main_counter);
        }
      ELSE
        {
        }
        ENDIF
      }
      ENDIF
    // The first token_location that the parser establishes is caused by the
    // parser scanning all of the lines in the source code.  This messes up the
    // logic for backing up one line, which is needed to correctly step through
    // COBOL code with GDB-COBOL.  So, we clear it here.
    current_location_minus_one_clear();

    // It is at this point that we check to see if the call to this function
    // is a re-entry because of an ENTRY statement:

    IF(var_decl_entry_index, ne_op, size_t_zero_node)
      {
      // This is an ENTRY re-entry.  The processing of USING variables was
      // done in parser_entry, so now we jump to the switch statement
      gg_append_statement(current_function->entry_switch_goto);
      }
    ELSE
      {
      }
    ENDIF
    current_function->pseudo_return_index =
                gg_define_variable(SIZE_T, "_pseudo_return_index", vs_static);

    // Establish the formal parameters from the USING clause.
    establish_using(nusing, args);

    current_function->initialized = true;
    }
  }

void
parser_logop( struct cbl_field_t *tgt,
              struct cbl_field_t *a, // Is NULL for single-valued ops
              enum logop_t logop,
              struct cbl_field_t *b )
  {
  Analyze();
  SHOW_PARSE
    {
    if( logop == true_op)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_FIELD(" ", tgt)
      SHOW_PARSE_TEXT(" will be set to TRUE ")
      }
    else if( logop == false_op)
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_FIELD(" ", tgt)
      SHOW_PARSE_TEXT(" will be set to FALSE ")
      }
    else
      {
      SHOW_PARSE_HEADER
      SHOW_PARSE_FIELD(" ", tgt)
      SHOW_PARSE_TEXT(" = ")
      if( a )
        {
        SHOW_PARSE_FIELD("", a)
        }
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT( cbl_logop_str(logop) )
      if( b )
        {
        SHOW_PARSE_FIELD(" ", b)
        }
      }
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  switch(logop)
    {
    case and_op:
    case or_op:
    case xor_op:
    case xnor_op:
    case not_op:
      CHECK_FIELD(b);
      break;
    default:
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("operation: ", cbl_logop_str(logop), "")
    TRACE1_END
    if( logop != true_op )
      {
      if( a )
        {
        TRACE1_INDENT
        TRACE1_FIELD("operand A: ", a, "");
        }
      TRACE1_INDENT
      if( b )
        {
        TRACE1_FIELD("operand B: ", b, "");
        }
      TRACE1_END
      }
    }

  // This routine takes two conditionals and a logical operator.  From those,
  // it creates and returns another conditional:

  if( tgt->type != FldConditional )
    {
    cbl_internal_error("%<parser_logop()%> was called with variable %s on line %d"
          ", which is not a FldConditional",
          tgt->name, cobol_location().first_line);
    }
  if( a && a->type != FldConditional )
    {
    cbl_internal_error("%<parser_logop()%> was called with variable %s on line %d"
          ", which is not a FldConditional",
          a->name, cobol_location().first_line);
    }
  if( b && b->type != FldConditional )
    {
    cbl_internal_error("%<parser_logop()%> was called with variable %s on line %d"
          ", which is not a FldConditional",
          b->name, cobol_location().first_line);
    }

  switch( logop )
    {
    case and_op:
      tgt->var_decl_node = gg_build_logical_expression(
                        a->var_decl_node,
                        and_op,
                        b->var_decl_node);
      break;

    case or_op:
      tgt->var_decl_node = gg_build_logical_expression(
                  a->var_decl_node,
                  or_op,
                  b->var_decl_node);
      break;

    case not_op:
      tgt->var_decl_node = gg_build_logical_expression(
                  NULL,
                  not_op,
                  b->var_decl_node);
      break;

    case xor_op:
      tgt->var_decl_node = gg_build_logical_expression(
                  a->var_decl_node,
                  xor_op,
                  b->var_decl_node);
      break;

    case xnor_op:
      {
      tgt->var_decl_node =
                  gg_build_logical_expression(a->var_decl_node,
                                              xor_op,
                                              b->var_decl_node);

      // I need to negate the result.

      tgt->var_decl_node = gg_build_logical_expression(
                  NULL,
                  not_op,
                  tgt->var_decl_node);
      }
    break;

    case true_op:
      tgt->var_decl_node = boolean_true_node;
      break;

    case false_op:
      tgt->var_decl_node = boolean_false_node;
      break;
    }

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_TEXT_ABC("result: ", tgt->name, "")
    TRACE1_FIELD_VALUE("", tgt, "")
    TRACE1_END
    }
  }

void
parser_relop(   cbl_field_t *tgt,
                cbl_refer_t aref,
                enum relop_t relop,
                cbl_refer_t bref )
  {
  Analyze();
  cbl_field_t *a = aref.field, *b = bref.field;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_REF(" = ", aref)
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(relop_str(relop))
    SHOW_PARSE_REF(" ", bref)
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  CHECK_FIELD(a);
  CHECK_FIELD(b);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("operation: ", relop_str(relop), "")
    TRACE1_INDENT
    TRACE1_REFER("operand A: ", aref, "");
    TRACE1_INDENT
    TRACE1_REFER("operand B: ", bref, "");
    }

  // This routine builds the relational expression and returns the TREE as
  // a conditional:

  if( tgt->type != FldConditional )
    {
    cbl_internal_error("%<parser_relop%> was called with variable %qs, "
                       "which is not a FldConditional",
                       tgt->name);
    }

  tree left;
  tree right;
  cobol_compare(left, right, aref, bref);
  tgt->var_decl_node = gg_build_relational_expression(left,
                                                      relop,
                                                      right);
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_relop_long(cbl_field_t *tgt,
                  long avalue,
                  enum relop_t relop,
                  cbl_refer_t bref )
  {
  Analyze();
  // We are comparing a long to a field, so the field had best be numerical

  cbl_field_t *b = bref.field;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_TEXT(" = <long value> ")
    SHOW_PARSE_TEXT(relop_str(relop))
    SHOW_PARSE_REF(" ", bref)
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  CHECK_FIELD(b);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT_ABC("operation: ", relop_str(relop), "")
    TRACE1_INDENT
    char ach[32];
    sprintf(ach, "operand A: %ld (long value) ", avalue);
    TRACE1_TEXT(ach);
    TRACE1_INDENT
    TRACE1_REFER("operand B: ", bref, "");
    }

  // This routine builds the relational expression and returns the TREE as
  // a conditional:
  if( tgt->type != FldConditional )
    {
    cbl_internal_error("%<parser_relop()%> was called with variable %s, "
          "which is not a FldConditional",
          tgt->name);
    }

  tree tree_a  = build_int_cst_type(LONG, avalue);
  tree tree_b;
  get_binary_value( tree_b, bref.field, LONG);
  tree comp_res = gg_define_variable(LONG);
  gg_assign(comp_res, gg_subtract(tree_a, tree_b));

  // comp_res is negative, zero, position for less-than, equal-to, greater-than

  // So, we simply compare the result of the comparison to zero using the relop
  // we were given to turn it into a TRUE/FALSE
  gg_assign(  tgt->var_decl_node,
              gg_build_relational_expression( comp_res,
                                            relop,
                                            gg_cast(LONG, integer_zero_node)));
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_if( struct cbl_field_t *conditional )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", conditional)
    SHOW_PARSE_END
    }

  CHECK_FIELD(conditional);

  if( conditional->type != FldConditional )
    {
    cbl_internal_error("%<parser_if()%> was called with variable %s, "
          "which is not a FldConditional",
          conditional->name);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("testing: ")
    TRACE1_TEXT(conditional->name)
    TRACE1_FIELD_VALUE("", conditional, "")
    TRACE1_END
    }

  gg_create_true_false_statement_lists(conditional->var_decl_node);
  }

// The following routines border on abuse of the preprocessor, if not the
// programmer who is trying to understand this.  Look at the #defines in
// gengen.h, and check out the comments for gg_if in gengen.c

void
parser_else(void)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  ELSE
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("taking FALSE branch")
    TRACE1_END
    }
  }

void
parser_fi(void)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  ENDIF
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  }

void
parser_see_stop_run(struct cbl_refer_t exit_status,
                    const char *message)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( exit_status.field )
      {
      SHOW_PARSE_FIELD(" ERROR STATUS ", exit_status.field);
      }
    SHOW_PARSE_END
    }
  if( message )
    {
    parser_display_literal(message, DISPLAY_ADVANCE);
    }
  TRACE1
    {
    TRACE1_HEADER
    }

  // It's a stop run.  Return return-code to the operating system:
  tree returned_value;
  if( exit_status.field )
    {
    // There is an exit_status, so it wins:
    get_binary_value( returned_value, exit_status.field, INT);
    TRACE1
      {
      TRACE1_REFER(" exit_status ", exit_status, "")
      }
    }
  else
    {
    returned_value = gg_define_variable(INT);
    gg_assign(returned_value, gg_cast(INT, current_function->var_decl_return));
    TRACE1
      {
      gg_fprintf( trace_handle,
                  2,
                  "RETURN-CODE %s [%d]",
                  gg_string_literal(cbl_field_of(
                                    symbol_at(return_code_register()))->name),
                  returned_value);
      }
    }
  TRACE1
    {
    gg_printf(" gg_exit(%d)\n", returned_value, NULL_TREE);
    TRACE1_END
    }
  gg_exit(returned_value);
  }

void
parser_label_label(struct cbl_label_t *label)
  {
  label->lain = cobol_location().first_line;
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL("", label)
    char ach[32];
    sprintf(ach, " label is at %p", static_cast<void*>(label));
    SHOW_PARSE_TEXT(ach)
    if( label )
      {
      sprintf(ach,
              " label->proc is %p",
              static_cast<void*>(label->structs.proc));
      }
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("Establish label: ", label, "")
    TRACE1_END
    }

  RETURN_WHEN_HIJACKED;

  CHECK_LABEL(label);

  label_verify.lay(label);

  if(strcmp(label->name, "_end_declaratives") == 0 )
    {
    suppress_cobol_entry_point = false;
    }
  gg_append_statement( label_fetch(label)->label );
  }

void
parser_label_goto(struct cbl_label_t *label)
  {
  label->used = yylineno;

  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL(" ", label)
    char ach[32];
    sprintf(ach, " label is at %p", static_cast<void*>(label));
    SHOW_PARSE_TEXT(ach)
    if( label )
      {
      sprintf(ach,
              " label->proc is %p",
              static_cast<void*>(label->structs.proc));
      }
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_LABEL("GOTO label: ", label, "")
    TRACE1_END
    }

  RETURN_WHEN_HIJACKED;

  CHECK_LABEL(label);

  label_verify.go_to(label);

  label_verify.go_to(label);

  if( strcmp(label->name, "_end_declaratives") == 0 )
    {
    suppress_cobol_entry_point = true;
    }

  gg_append_statement( label_fetch(label)->go_to );
  }

void
parser_setop( struct cbl_field_t *tgt,
              struct cbl_field_t *candidate,
              enum setop_t op,
              struct cbl_field_t *domain)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_FIELD(" = ", candidate)
    if( op == is_op )
      {
      SHOW_PARSE_TEXT(" is_op ")
      }
    SHOW_PARSE_FIELD(" = ", domain)
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);
  CHECK_FIELD(candidate);
  CHECK_FIELD(domain);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("parser_setop: ", candidate, "")
    TRACE1_TEXT(" ")
    TRACE1_TEXT(setop_str(op))
    TRACE1_FIELD(" ", domain, "")
    TRACE1_END
    }

  gcc_assert(tgt->type == FldConditional);

  switch(op)
    {
    case is_op:
      switch(candidate->type)
        {
        case FldGroup:
        case FldAlphanumeric:
          gg_assign(tgt->var_decl_node, gg_build_relational_expression(
                      gg_call_expr(INT,
                                   "__gg__setop_compare",
                                   gg_get_address_of(candidate->var_decl_node),
                                   member(domain, "initial"),
                                   NULL_TREE),
                      ne_op,
                      integer_zero_node));
          break;
        default:
          dbgmsg("%10s in %s:%d", __func__, __FILE__, __LINE__ );
          cbl_internal_error("candidate %s has unimplemented %<CVT_type%> %d(%s)",
                             candidate->name,
                             candidate->type,
                             cbl_field_type_str(candidate->type));
          gcc_unreachable();
          break;
        }
      break;

    default:
      dbgmsg("%10s in %s:%d", __func__, __FILE__, __LINE__ );
      cbl_internal_error("unknown %<setop_t%> code %d", op);
      gcc_unreachable();
      break;
    }
  }

void
parser_classify(    cbl_field_t *tgt,
               const cbl_refer_t  &candidate,
                    enum classify_t type )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    SHOW_PARSE_FIELD(" = ", candidate.field)
    SHOW_PARSE_TEXT(" IS ")
    SHOW_PARSE_TEXT(classify_str(type))
    SHOW_PARSE_END
    }

  gcc_assert(tgt->type == FldConditional);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER_VALUE("parser_classify: ", candidate, "")
    TRACE1_TEXT(" ")
    TRACE1_TEXT(classify_str(type))
    }

  gg_assign(tgt->var_decl_node, gg_build_relational_expression(
              gg_call_expr(INT,
                           "__gg__classify",
                           build_int_cst_type(INT, type),
                           gg_get_address_of(candidate.field->var_decl_node),
                           refer_offset(candidate),
                           refer_size_dest(candidate),
                           NULL_TREE),
              ne_op,
              integer_zero_node));

  TRACE1
    {
    TRACE1_TEXT(" result is ")
    TRACE1_TEXT(tgt->name)
    TRACE1_FIELD_VALUE(" -> ", tgt, "")
    TRACE1_END
    }
  }

void
parser_perform(const cbl_perform_tgt_t *tgt, const cbl_refer_t &how_many)
  {
  const cbl_field_t *N = how_many.field;
  // No SHOW_PARSE here; we want to fall through:
  if( !tgt->to() )
    {
    // We only have tgt->from.
    if( !N )
      {
      // There is no N.  This is a simple PERFORM proc-1
      parser_perform(tgt->from());
      }
    else
      {
      // This is a PERFORM proc-1 N TIMES
      parser_perform_times(tgt->from(), how_many);
      }
    }
  else
    {
    // We have both from and to
    if( !N )
      {
      // There is no N.  This is PERFORM proc-1 THROUGH proc-2
      // false means nexting in GDB will work
      internal_perform_through(tgt->from(), tgt->to(), false);
      }
    else
      {
      // This is a PERFORM proc-1 THROUGH proc-2 N TIMES
      internal_perform_through_times(tgt->from(), tgt->to(), how_many);
      }
    }
  }

static void
create_iline_address_pairs(struct cbl_perform_tgt_t *tgt)
  {
  gg_create_goto_pair(&tgt->addresses.top.go_to,
                      &tgt->addresses.top.label);

  gg_create_goto_pair(&tgt->addresses.exit.go_to,
                      &tgt->addresses.exit.label);

  gg_create_goto_pair(&tgt->addresses.test.go_to,
                      &tgt->addresses.test.label);

  gg_create_goto_pair(&tgt->addresses.testA.go_to,
                      &tgt->addresses.testA.label);

  gg_create_goto_pair(&tgt->addresses.setup.go_to,
                      &tgt->addresses.setup.label);
  }

void
parser_perform_start( struct cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( tgt )
      {
      SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
      char ach[32];
      sprintf(ach, " %p", static_cast<void*>(tgt));
      SHOW_PARSE_TEXT(ach);
      SHOW_PARSE_LABEL(" ", tgt->from())
      if( tgt->to() )
        {
        SHOW_PARSE_LABEL(" ", tgt->to())
        }
      }
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    if( tgt->from() )
      {
      TRACE1_LABEL(" from ", tgt->from(), "")
      }
    if( tgt->to() )
      {
      TRACE1_LABEL(" to ", tgt->to(), "")
      }
    TRACE1_END
    }

  // Create the goto/label pairs we are going to be needing:
  create_iline_address_pairs(tgt);

  // From here we have to jump to the loop setup code:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("GOTO SETUP")
    SHOW_PARSE_END
    }
  gg_append_statement(tgt->addresses.setup.go_to);

  // The next parser+_generated instructions will be the body of the loop, so we
  // need a TOP label here so we can get back to them:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL TOP:")
    SHOW_PARSE_END
    }

  // Give GDB-COBOL something to chew on when NEXTing.  This instruction will
  // get the line number of the PERFORM N TIMES code.
  gg_append_statement(tgt->addresses.top.label);
  // Necessary for GDB-COBOL PERFORM <inline> processing.
  insert_nop(105);
  }

void
parser_perform_conditional( struct cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
    char ach[32];
    sprintf(ach, " %p", static_cast<void*>(tgt));
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  unsigned int i = tgt->addresses.number_of_conditionals;

  if( !(i < MAXIMUM_UNTILS) )
    {
    cbl_internal_error("%s:%d: %u exceeds %<MAXIMUM_UNTILS%> of %d, line %d",
                       __func__, __LINE__,
                       i, MAXIMUM_UNTILS, CURRENT_LINE_NUMBER);
    }
  gcc_assert(i < MAXIMUM_UNTILS);

  // Create an unnamed goto/label pair for jumping over the conditional
  // calculation.
  gg_create_goto_pair(&tgt->addresses.condover[i].go_to,
                      &tgt->addresses.condover[i].label);

  // Create an unnamed goto/label pair for jumping into the
  // conditional calculation:
  gg_create_goto_pair(&tgt->addresses.condinto[i].go_to,
                      &tgt->addresses.condinto[i].label);

  // Create an unnamed goto/label pair for jumping back from the
  // conditional calculation:
  gg_create_goto_pair(&tgt->addresses.condback[i].go_to,
                      &tgt->addresses.condback[i].label);

  // The next instructions that the parser will give us are the conditional
  // calculation, so the first thing that goes down is the condover:
  /* The following NOP is needed to make NEXT OVER PERFORM BEFORE/AFTER UNTIL
     behaves properly.  */
  insert_nop(106);
  gg_append_statement(tgt->addresses.condover[i].go_to);

  // And then, of course, we need to be able to jump back here to actually
  // do the run-time conditional calculations:
  gg_append_statement(tgt->addresses.condinto[i].label);

  tgt->addresses.number_of_conditionals += 1;
  }

void
parser_perform_conditional_end( struct cbl_perform_tgt_t *tgt )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
    char ach[32];
    sprintf(ach, " %p", static_cast<void*>(tgt));
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  unsigned int i = tgt->addresses.number_of_conditionals;
  gcc_assert(i);

  // We need to cap off the prior conditional in this chain of conditionals
  gg_append_statement(tgt->addresses.condback[i-1].go_to);
  gg_append_statement(tgt->addresses.condover[i-1].label);
  }

static void
build_N_pairs(tree *go_to, tree *label, size_t N)
  {
  for(size_t i=0; i<N; i++)
    {
    tree a;
    tree b;
    gg_create_goto_pair(&a, &b);
    go_to[i] = a;
    label[i] = b;
    }
  }

static void
perform_outofline_before_until(struct cbl_perform_tgt_t *tgt,
                               bool /*test_before*/,
                               size_t /*N*/,
                               struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [through proc-2] TEST BEFORE} UNTIL

  /*
      TOP:
         GOTO condinto
         condback:
            IF CONDITION 0
                GOTO EXIT
            ELSE
                EXECUTE BODY
                GOTO TOP
      EXIT:

      GOTO jumpover
         condinto:
         <conditional calculation>
         GOTO condback
      jumpover:
  */

  create_iline_address_pairs(tgt);

  // Tag the top of the perform

  gg_append_statement(tgt->addresses.top.label);

  // Go do the conditional calculation:

  gg_append_statement(tgt->addresses.condinto[0].go_to);

  // And put down the label so that the conditional calculation knows
  // where to return:
  gg_append_statement(tgt->addresses.condback[0].label);

  perform_is_armed = CURRENT_LINE_NUMBER ;

  parser_if(varys[0].until);
    {
    // We're done, so leave
    gg_append_statement(tgt->addresses.exit.go_to);
    }
  parser_else();
    {
    // We're not done, so execute the body
    // true means GDB next will fall through
    internal_perform_through(tgt->from(), tgt->to(), true);

    // Jump back to the test:
    gg_append_statement(tgt->addresses.top.go_to );
    }
  parser_fi();

  // Label the bottom of the PERFORM
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_outofline_after_until(struct cbl_perform_tgt_t *tgt,
                              bool /*test_before*/,
                              size_t /*N*/,
                              struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [through proc-2] TEST AFTER UNTIL

  /*
      TOP:
          EXECUTE BODY
          GOTO condinto
          condback:
          IF CONDITION 0
              GOTO EXIT
          ELSE
              GOTO TOP
      EXIT:

      GOTO jumpover
         condinto:
         <conditional calculation>
         GOTO condback
      jumpover:
  */

  perform_is_armed = CURRENT_LINE_NUMBER ;

  create_iline_address_pairs(tgt);

  // Label the top of the loop
  gg_append_statement(tgt->addresses.top.label);

  // Build the perform:
  // true in the next call means that GDB next will not stop until the entire
  // until loop is finished
  internal_perform_through(tgt->from(), tgt->to(), true);

  // Go recalculate the conditional:
  gg_append_statement( tgt->addresses.condinto[0].go_to);

  // And lay down the label for the come-back from the recalculation:
  gg_append_statement( tgt->addresses.condback[0].label);

  // Assess the conditional
  parser_if(varys[0].until);
  // It's true, so we're done
  gg_append_statement( tgt->addresses.exit.go_to );
  parser_else();
  // It's false, so execute the body again
  gg_append_statement( tgt->addresses.top.go_to );
  parser_fi();
  // Label the bottom of the PERFORM
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_outofline_testafter_varying(struct cbl_perform_tgt_t *tgt,
                                    bool /*test_before*/,
                                    size_t N,
                                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [THROUGH proc-2] TEST AFTER VARYING

  /*

  [ENTRANCE]
              MOVE FROM_0 TO VARYING_0
  INIT_1:
              MOVE FROM_1 TO VARYING_1
  INIT_2:
              MOVE FROM_2 TO VARYING_2
  . . . . . . . . . . . . . . . . . .
  INIT_N-2:
              MOVE FROM_N-2 TO VARYING_N-2
  INIT_N-1:
              MOVE FROM_N-1 TO VARYING_N-1
              GOTO TOP
  TOP:
              PERFORM PROC-1 [THROUGH PROC-2]
              IF NOT CONDITION_N-1
                  ADD BY_N-1 TO VARYING_N-1
                  GOTO TOP
              IF NOT CONDITION_N-2
                  ADD BY_N-2 TO VARYING_N-2
                  GOTO INIT_N-1
              IF NOT CONDITION_N-3
                  ADD BY_N-3 TO VARYING_N-3
                  GOTO INIT_N-2
  . . . . . . . . . . . . . . . . . .
              IF NOT CONDITION_1
                  ADD BY_1 TO VARYING_1
                  GOTO INIT_2
              IF NOT CONDITION_0
                  ADD BY_0 TO VARYING_0
                  GOTO INIT_1
  EXIT:

  */

  // So, we're going to do that.  But because the initializations
  // and the testing are so nicely loopish, we're going to let
  // the computer create them for us.

  // We are going to need a set of N label pairs.  Actually, we
  // only need N-1; we don't use the zeroth pair.  But the code
  // is cleaner if we just build all N of them.

  perform_is_armed = CURRENT_LINE_NUMBER ;

  create_iline_address_pairs(tgt);

  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];

  build_N_pairs(go_to, label, N);

  // Build the initialization section:
  for(size_t i=0; i<N; i++)
    {
    gg_append_statement(label[i]);
    parser_move(varys[i].varying, varys[i].from);
    }
  // These next two statements do nothing.  But it'll make sense
  // when we move the logic around to create an inline VARYING
  gg_append_statement(tgt->addresses.top.go_to);
  gg_append_statement(tgt->addresses.top.label);

  // Build the body:
  // true in the next call means that the entire loop will complete
  // even in the face of a GDB next
  internal_perform_through(tgt->from(), tgt->to(), true);

  // Build the test section
  // (The oddball test is because N is a size_t, and can't go negative)
  for(size_t i=N-1; i<N; i--)
    {
    // Jump to the conditional calculation:
    gg_append_statement( tgt->addresses.condinto[i].go_to);

    // And put down the label for the return from that calculation:
    gg_append_statement( tgt->addresses.condback[i].label);

    parser_if( varys[i].until );
    // Condition is true; so we'll fall through
    parser_else();
    // Condition is false, so we increment, and keep going:
    parser_add(varys[i].varying, varys[i].by, varys[i].varying);
    if( i == N-1 )
      {
      gg_append_statement(tgt->addresses.top.go_to);
      }
    else
      {
      gg_append_statement(go_to[i+1]);
      }
    parser_fi();
    }
  // Arriving here means that we all of the conditions were
  // true.  So, we're done.
  }

static void
perform_outofline_before_varying(   struct cbl_perform_tgt_t *tgt,
                                    bool /*test_before*/,
                                    size_t N,
                                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [THROUGH proc-2] TEST BEFORE VARYING

  /*

  ENTRANCE:
              SET ALL VARYING-N to FROM-N
  TEST_0:
              IF CONDITION_0:
                  GOTO EXIT:
  TEST_1:
              IF CONDITION_1:
                  ADD BY_0 TO VARYING_0
                  MOVE FROM_1 TO VARYING_1
                  GOTO TEST_0
  TEST_2:
              IF CONDITION_2:
                  ADD BY_1 TO VARYING_1:
                  MOVE FROM_2 TO VARYING_2
                  GOTO TEST_1:
  TEST_3:
              IF CONDITION_3:
                  ADD BY_2 TO VARYING_2:
                  MOVE FROM_3 TO VARYING_3
                  GOTO TEST_1:
  . . . . . . . . . . . . . . . .
  TEST_N-1:
              IF CONDITION_N-1:
              ADD BY_N-2 TO VARYING_N-2:
              MOVE FROM_N-2 TO VARYING_N-2
              GOTO TEST_N-2
  TOP:
              PERFORM proc-1 [THROUGH proc-2]

              ADD BY_N-1 TO VARYING_N-1:
              GOTO TEST_N-1

  */
  create_iline_address_pairs(tgt);

  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];
  build_N_pairs(go_to, label, N);

  perform_is_armed = CURRENT_LINE_NUMBER ;

  // Initialize all varying:

  for(size_t i=0; i<N; i++)
    {
    parser_move(varys[i].varying, varys[i].from);
    }

  // Lay down the testing cycle:
  for(size_t i=0; i<N; i++)
    {
    // This is the chain of conditions that gets tested before
    // the statements run.  Each condition gets its own label.
    gg_append_statement(label[i]);

    // go back to the instructions that calculate the conditional
    gg_append_statement(tgt->addresses.condinto[i].go_to);

    // And put down the label that brings us back:
    gg_append_statement(tgt->addresses.condback[i].label);

    // Now we can test the calculated conditional:
    parser_if(varys[i].until);
    // This condition has been met, so we increment the
    // variable to the left, reset ours, and go check the
    // one we just incremented
    if(i == 0)
      {
      // This is the leftmost condition condition, so when it
      // is TRUE, we are done.
      gg_append_statement(  tgt->addresses.exit.go_to );
      }
    else
      {
      // This is one of the conditions to the right of the
      // first one.  So, we augment the VARYING to the
      // left, reset our VARYING, and go test the
      // condition to the left:
      parser_add(varys[i-1].varying, varys[i-1].by, varys[i-1].varying);
      parser_move(varys[i].varying, varys[i].from);
      gg_append_statement( go_to[i-1] );
      }
    parser_else();
    // This condition has not been met.
    if( i == N-1 )
      {
      // ... and this is the rightmost condition
      // This is where we perform the body of the PERFORM.
      gg_append_statement(  tgt->addresses.top.label );

      // Build the body:
      // true in the next call means that GDB NEXT will pass through the
      // entire loop
      internal_perform_through(tgt->from(), tgt->to(), true);

      // And now we augment FROM_N-1 by BY__N-1
      parser_add(varys[N-1].varying, varys[N-1].by, varys[N-1].varying);

      // And we jump back to test that freshly-augmented condition
      gg_append_statement( go_to[N-1] );
      }
    else
      {
      // At this point, a condition that is not the rightmost
      // one has not been met.  We could, in principle, just
      // fall through at this point.  But that makes me nervous.
      // So, I am going to put in what may well be an
      // unnecessary goto:
      gg_append_statement( go_to[i+1] );
      }
    parser_fi();
    }
  // The astute observer will have noted that there is no way
  // for the generated runtime code to reach this point except by jumpint to
  // the EXIT: label.
  // We have, you see, reached the egress:
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_outofline(  struct cbl_perform_tgt_t *tgt,
                    bool test_before,
                    size_t N,
                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is an out-of-line perform.

  // We need to create the address pairs, because there was no parser_perform_start

  if( N == 1 && !varys[0].varying.field )
    {
    // There is no varys.varying, so this is just a PERFORM proc-1 UNTIL
    if( test_before )
      {
      perform_outofline_before_until(tgt, test_before, N, varys);
      }
    else
      {
      perform_outofline_after_until(tgt, test_before, N, varys);
      }
    }
  else
    {
    // This is a PERFORM proc-1 [through proc-2] VARYING
    if( test_before )
      {
      perform_outofline_before_varying(tgt, test_before, N, varys);
      }
    else
      {
      perform_outofline_testafter_varying(tgt, test_before, N, varys);
      }
    }
  }

static void
perform_inline_until(   struct cbl_perform_tgt_t *tgt,
                        bool test_before,
                        size_t /*N*/,
                        struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM <inline> [TEST {BEFORE|AFTER}] UNTIL

  /*

              GOTO SETUP
      TOP:    S1
              S2
              EXIT PERFORM -> GOTO EXIT:
              S3
              S4
              EXIT PERFORM CYCLE -> GOTO TEST
              S6
              S7
      TEST:   IF CONDITION
                  GOTO EXIT
              ELSE
                  GOTO TOP
      SETUP:
              IF TEST BEFORE
                  GOTO TEST
              ELSE
                  GOTO TOP
      EXIT:
  */
  gg_append_statement(tgt->addresses.test.label);

  // Go to where the conditional is recalculated....
  gg_append_statement(tgt->addresses.condinto[0].go_to);

  // ...and lay down the return address.
  gg_append_statement(tgt->addresses.condback[0].label);

  parser_if( varys[0].until );
  gg_append_statement(  tgt->addresses.exit.go_to );
  parser_else();
  gg_append_statement(  tgt->addresses.top.go_to );
  parser_fi();
  gg_append_statement(  tgt->addresses.setup.label );

  if( test_before )
    {
    gg_append_statement(  tgt->addresses.test.go_to );
    }
  else
    {
    gg_append_statement(  tgt->addresses.top.go_to );
    }
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_inline_testbefore_varying(  struct cbl_perform_tgt_t *tgt,
                                    bool /*test_before*/,
                                    size_t N,
                                    struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM proc-1 [THROUGH proc-2] TEST BEFORE VARYING

  /*

              GOTO SETUP
  TOP:
              S1
              S2
              EXIT PERFORM -- GOTO EXIT:
              S3
              S4
              EXIT PERFORM CYCLE -- GOTO TESTA
              S5
              S6
              GOTO AUGMENT_N-1
  SETUP:
              SET ALL VARYING-N to FROM-N
  TEST_0:
              IF CONDITION_0:
                  GOTO EXIT:
  TEST_1:
              IF CONDITION_1:
                  ADD BY_0 TO VARYING_0
                  MOVE FROM_1 TO VARYING_1
                  GOTO TEST_0
  TEST_2:
              IF CONDITION_2:
                  ADD BY_1 TO VARYING_1:
                  MOVE FROM_2 TO VARYING_2
                  GOTO TEST_1:
  TEST_3:
              IF CONDITION_3:
                  ADD BY_2 TO VARYING_2:
                  MOVE FROM_3 TO VARYING_3
                  GOTO TEST_1:
  . . . . . . . . . . . . . . . .
  TEST_N-1:
              IF CONDITION_N-1:
                  ADD BY_N-2 TO VARYING_N-2:
                  MOVE FROM_N-2 TO VARYING_N-2
                  GOTO TEST_N-2

              GOTO TOP
  TESTA:
              ADD BY_N-1 TO VARYING_N-1:
              GOTO TEST_N-1

  */
  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];
  build_N_pairs(go_to, label, N);

  // At this point in the executable, the body of the inline loop has been
  // laid down, so we lay down a GOTO TESTA
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("GOTO TESTA")
    SHOW_PARSE_END
    }
  gg_append_statement(tgt->addresses.testA.go_to);

  // It's now safe to setup the whole extravaganza of UNTIL conditions:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL SETUP:")
    SHOW_PARSE_END
    }
  gg_append_statement(tgt->addresses.setup.label);

  // Initialize all varying:
  for(size_t i=0; i<N; i++)
    {
    parser_move(varys[i].varying, varys[i].from);
    }

  // Lay down the testing cycle:
  for(size_t i=0; i<N; i++)
    {
    // This is the chain of conditions that gets tested before
    // the statements run.  Each condition gets its own label.
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach[32];
      sprintf(ach, "LABEL [" HOST_SIZE_T_PRINT_DEC "]:", (fmt_size_t)i);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_END
      }
    gg_append_statement(label[i]);

    // Jump to where the conditional is calculated...
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach[32];
      sprintf(ach, "LABEL CONDINTO[" HOST_SIZE_T_PRINT_DEC "]:",
              (fmt_size_t)i);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_END
      }
    gg_append_statement(tgt->addresses.condinto[i].go_to);

    // ...and lay down the label for the return from there
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      char ach[32];
      sprintf(ach, "LABEL CONDBACK[" HOST_SIZE_T_PRINT_DEC "]:",
              (fmt_size_t)i);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_END
      }
    gg_append_statement(tgt->addresses.condback[i].label);
    // Needed to make GDB NEXT over PERFORM in-line VARYING UNTIL work
    // predictably.
    insert_nop(107);

    // Test that conditional
    parser_if(varys[i].until);
    // This condition has been met, so we increment the
    // variable to the left, reset ours, and go check the
    // one we just incremented
    if(i == 0)
      {
      // This is the leftmost condition condition, so when it
      // is TRUE, we are done.
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("GOTO EXIT")
        SHOW_PARSE_END
        }
      gg_append_statement(  tgt->addresses.exit.go_to );
      }
    else
      {
      // This is one of the conditions to the right of the
      // first one.  So, we augment the VARYING to the
      // left, reset our VARYING, and go test the
      // condition to the left:
      parser_add(varys[i-1].varying, varys[i-1].by, varys[i-1].varying);
      parser_move(varys[i].varying, varys[i].from);
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char ach[32];
        sprintf(ach, "GOTO [" HOST_SIZE_T_PRINT_DEC "]:",
                (fmt_size_t)(i-1));
        SHOW_PARSE_TEXT(ach)
        SHOW_PARSE_END
        }
      gg_append_statement( go_to[i-1] );
      }
    parser_else();
    // This condition has not been met.
    if( i == N-1 )
      {
      // ... and this is the rightmost condition
      // This is where we perform the body of the PERFORM.
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("GOTO TOP")
        SHOW_PARSE_END
        }
      gg_append_statement(  tgt->addresses.top.go_to );

      // And now we augment FROM_N-1 by BY__N-1
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        SHOW_PARSE_TEXT("LABEL TESTA:")
        SHOW_PARSE_END
        }
      gg_append_statement(tgt->addresses.testA.label);
      parser_add(varys[N-1].varying, varys[N-1].by, varys[N-1].varying);
      // And we jump back to test that freshly-augmented condition
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char ach[32];
        sprintf(ach, "GOTO [" HOST_SIZE_T_PRINT_DEC "]:",
                (fmt_size_t)(N-1));
        SHOW_PARSE_TEXT(ach)
        SHOW_PARSE_END
        }
      gg_append_statement( go_to[N-1] );
      }
    else
      {
      // At this point, a condition that is not the rightmost
      // one has not been met.  We could, in principle, just
      // fall through at this point.  But that makes me nervous.
      // So, I am going to put in what may well be an
      // unnecessary goto:
      SHOW_PARSE
        {
        SHOW_PARSE_INDENT
        char ach[32];
        sprintf(ach, "GOTO [" HOST_SIZE_T_PRINT_DEC "]:",
                (fmt_size_t)(i-1));
        SHOW_PARSE_TEXT(ach)
        SHOW_PARSE_END
        }
      gg_append_statement( go_to[i+1] );
      }
    parser_fi();
    }

  // The astute observer will have noted that there is no way
  // for the generated runtime code to reach this point.
  //
  // We have, you see, reached the egress:
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_inline_testafter_varying(  struct cbl_perform_tgt_t *tgt,
                                   bool /*test_before*/,
                                   size_t N,
                                   struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is a PERFORM <inline> TEST AFTER VARYING

  /*

              GOTO SETUP
  TOP:
              S1
              S2
              EXIT PERFORM -- GOTO EXIT:
              S3
              S4
              EXIT PERFORM CYCLE -- GOTO TESTA
              S5
              S6
              GOTO TESTA:

  SETUP:
              MOVE FROM_0 TO VARYING_0
  INIT_1:
              MOVE FROM_1 TO VARYING_1
  INIT_2:
              MOVE FROM_2 TO VARYING_2
  . . . . . . . . . . . . . . . . . .
  INIT_N-2:
              MOVE FROM_N-2 TO VARYING_N-2
  INIT_N-1:
              MOVE FROM_N-1 TO VARYING_N-1
              GOTO TOP
  TESTA:
  TEST_N-1:
              IF NOT CONDITION_N-1
                  ADD BY_N-1 TO VARYING_N-1
                  GOTO TOP
              IF NOT CONDITION_N-2
                  ADD BY_N-2 TO VARYING_N-2
                  GOTO INIT_N-1
              IF NOT CONDITION_N-3
                  ADD BY_N-3 TO VARYING_N-3
                  GOTO INIT_N-2
  . . . . . . . . . . . . . . . . . .
              IF NOT CONDITION_1
                  ADD BY_1 TO VARYING_1
                  GOTO INIT_2
              IF NOT CONDITION_0
                  ADD BY_0 TO VARYING_0
                  GOTO INIT_1
              // At this point, all conditions are true
  EXIT:

  */

  // So, we're going to do that.  But because the initializations
  // and the testing are so nicely loopish, we're going to let
  // the computer create them for us.

  // We are going to need a set of N label pairs.  Actually, we
  // only need N-1; we don't use the zeroth pair.  But the code
  // is cleaner if we just build all N of them.

  tree go_to[MAX_AFTERS];
  tree label[MAX_AFTERS];

  build_N_pairs(go_to, label, N);

  // At this point the code being laid down, the GOTO SETUP was created,
  // followed by the stream of statements.  We terminate it with a
  // goto testa
  gg_append_statement(tgt->addresses.testA.go_to);

  // See the comment in create_iline_address_pairs()
  //gg_force_line_number(tgt->addresses.line_number_of_setup_code-1);

  // That's followed by the SETUP target:
  gg_append_statement(tgt->addresses.setup.label);

  // We now build the initialization section,
  for(size_t i=0; i<N; i++)
    {
    gg_append_statement(label[i]);
    parser_move(varys[i].varying, varys[i].from);
    }

  // Having done all the initialization, we jump back to the start of
  // the list of statements:
  gg_append_statement(tgt->addresses.top.go_to);

  // The list of statements ends with a goto TESTA, and that;s here:
  gg_append_statement(tgt->addresses.testA.label);

  // Build the test section
  // (The oddball test is because N is a size_t, and can't go negative)
  for(size_t i=N-1; i<N; i--)
    {
    // Jump to where the conditional is calculated...
    gg_append_statement(tgt->addresses.condinto[i].go_to);

    // ...and lay down the label to get back from there
    gg_append_statement(tgt->addresses.condback[i].label);

    // Test the newly-recalculated conditional:
    parser_if( varys[i].until );
    // Condition is true; so we'll fall through
    parser_else();
    // Condition is false, so we increment, and keep going:
    parser_add(varys[i].varying, varys[i].by, varys[i].varying);
    if( i == N-1 )
      {
      gg_append_statement(tgt->addresses.top.go_to);
      }
    else
      {
      gg_append_statement(go_to[i+1]);
      }
    parser_fi();
    }

  // Arriving here means that we all of the conditions were
  // true.  So, we're done.
  gg_append_statement(  tgt->addresses.exit.label );
  }

static void
perform_inline_impl( struct cbl_perform_tgt_t *tgt,
                     bool test_before,
                     size_t N,
                     struct cbl_perform_vary_t *varys )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  if( N == 1 && !varys[0].varying.field )
    {
    perform_inline_until(tgt, test_before, N, varys);
    }
  else
    {
    // This is a PERFORM proc-1 [through proc-2] VARYING
    if( !test_before )
      {
      perform_inline_testafter_varying(tgt, test_before, N, varys);
      }
    else
      {
      perform_inline_testbefore_varying(tgt, test_before, N, varys);
      }
    }
  }

void
parser_perform_until(   struct cbl_perform_tgt_t *tgt,
                        bool test_before,
                        size_t N,
                        struct cbl_perform_vary_t *varys )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" cbl_perform_tgt_t is at")
    char ach[32];
    sprintf(ach, " %p", static_cast<void*>(tgt));
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_LABEL(" ", tgt->from())
    if( tgt->to() )
      {
      SHOW_PARSE_LABEL(" THROUGH", tgt->to())
      }
    SHOW_PARSE_END
    }

  if( tgt->from()->type != LblLoop )
    {
    perform_outofline( tgt, test_before, N, varys);
    }
  else
    {
    perform_inline_impl( tgt, test_before, N, varys);
    }
  }

void
parser_perform_inline_times(struct cbl_perform_tgt_t *tgt,
                            struct cbl_refer_t how_many )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_LABEL("", tgt->from());
    SHOW_PARSE_REF(" how_many is ", how_many);
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD(" into ", how_many.field, " times");
    TRACE1_END
    }

  gcc_assert(tgt);

  tree counter = gg_define_variable(LONG);

  /*
              GOTO SETUP
  TOP:        S1
              EXIT PERFORM  --> GOTO EXIT
              S2
              EXIT PERFORM CYCLE --> GOTO TEST
              S3
  TESTA:
  TEST:       INCREMENT COUNTER
              IF COUNTER LT LIMIT
                  GOTO TOP
              ELSE
                  GOTO EXIT
  SETUP:      INITIALIZE COUNTER
              GOTO TOP
  EXIT:
  */

  // At this point, the GOTO SETUP, the label "TOP:" and the
  // body of the inline perform have been laid down.

  // Tack on the label for TEST and TESTA
  gg_append_statement( tgt->addresses.testA.label );
  gg_append_statement( tgt->addresses.test.label );

  gg_decrement(counter);
  // Do the test:
  IF( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    // We continue
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("If still counting GOTO TOP")
      SHOW_PARSE_END
      }
    gg_append_statement( tgt->addresses.top.go_to );
  ELSE
    // We are done
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("If count complete GOTO EXIT")
      SHOW_PARSE_END
      }
    gg_append_statement( tgt->addresses.exit.go_to );
    ENDIF

  // Lay down the SETUP: label
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL SETUP:")
    SHOW_PARSE_END
    }

  gg_append_statement( tgt->addresses.setup.label );

  // Get the count:
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("Access the how_many parameter")
    SHOW_PARSE_REF(" ", how_many)
    SHOW_PARSE_END
    }

  tree initial_value;
  get_binary_value(initial_value, how_many, LONG);
  gg_assign(counter, initial_value);

  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("GOTO TOP")
    SHOW_PARSE_END
    }

  // Make sure the initial count is valid:
  IF( counter, gt_op, gg_cast(LONG, integer_zero_node) )
    gg_append_statement( tgt->addresses.top.go_to );
  ELSE
    gg_append_statement( tgt->addresses.exit.go_to );
    ENDIF

  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("LABEL EXIT:")
    SHOW_PARSE_END
    }
  gg_append_statement( tgt->addresses.exit.label );
  }

void
parser_set_conditional88( const cbl_refer_t& refer, bool which_way )
  {
  Analyze();
  struct cbl_field_t *tgt = refer.field;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", tgt)
    if( which_way )
      {
      SHOW_PARSE_TEXT(" TRUE");
      }
    else
      {
      SHOW_PARSE_TEXT(" FALSE");
      }
    SHOW_PARSE_END
    }

  CHECK_FIELD(tgt);

  struct cbl_field_t *parent = parent_of(tgt);

  CHECK_FIELD(parent);

  cbl_domain_t *src;

  if( which_way )
    {
    src = tgt->data.domain_of();
    }
  else
    {
    src = tgt->data.false_value_of();
    }

  // We want to set the LEVEL88 target to TRUE (or FALSE), so we need to set
  // the parent of this LEVEL88 to the first element in data.domain (or
  // data.false_value);

  cbl_figconst_t figconst = cbl_figconst_of(src->first.name());

  if( !figconst )
    {
    // We are dealing with an ordinary string.

    size_t converted_bytes;
    const char *converted =
                  __gg__iconverter(parent->codeset.default_encodings.source->type,
                                   parent->codeset.encoding,
                                   src->first.name(),
                                   strlen(src->first.name())+1,
                                   &converted_bytes);
    gg_call(VOID,
            "__gg__refer_from_string",
            gg_get_address_of(parent->var_decl_node),
            size_t_zero_node,
            build_int_cst_type(SIZE_T, parent->data.capacity()),
            build_string_literal(converted_bytes, converted),
            NULL_TREE);
    }
  else
    {
    // This is a figurative constant
    gg_call(VOID,
            "__gg__parser_set_conditional",
            gg_get_address_of(parent->var_decl_node),
            build_int_cst_type(INT, figconst),
            NULL_TREE);
    }
  }

static
void set_user_status(struct cbl_file_t *file)
  {
  // This routine sets the user_status, if any, to the cblc_file_t::status

  // We have to do it this way, because in the case where the file->user_status
  // is in linkage, the memory addresses can end up pointing to the wrong
  // places
  if(file->user_status)
    {
    cbl_field_t *user_status = cbl_field_of(symbol_at(file->user_status));
    gcc_assert( user_status );
    gg_call(VOID,
            "__gg__set_user_status",
            gg_get_address_of(user_status->var_decl_node),
            gg_get_address_of(file->var_decl_node),
            NULL_TREE);
    }
  }

void
parser_file_add(struct cbl_file_t *file)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( file )
      {
      fprintf(stderr, " cbl_file_t: %s", file->name);
      if( file->record_length )
        {
        SHOW_PARSE_TEXT(" file->record_length is %s");
        SHOW_PARSE_TEXT(file->name);
        }
      else
        {
        SHOW_PARSE_TEXT(" file->record_length is ZERO")
        }
      }
    else
      {
      SHOW_PARSE_TEXT( " *file pointer is NULL")
      }
    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("%s: called with NULL *file", __func__);
    gcc_assert(file);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_add cbl_file_t ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }

  /*  The FD record can be flagged external.  Without definitive information, I
      am going to assume that the *everything* in the cblc_file_t structure is
      GLOBAL EXTERNAL.  If I have read the specification incorrectly, and it's
      possible for two programs to share a file connector but with, say, two
      different lists of keys, then the cblc_file_t structure will have to
      be changed to have one var_decl node for the common information, and a
      second one for local information.

      */

  gg_variable_scope_t scope;
  if( file->attr & external_e )
    {
    scope = vs_weak;
    }
  else
    {
    scope = vs_static;
    }

  char achName[2*sizeof(cbl_name_t)];

  // Use the global structure template declaration to produce the specific
  // structure definition expression:
  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_fc"); // For "File Connector"
  tree new_var_decl = gg_define_variable( cblc_file_type_node,
                                          achName,
                                          scope);

  // We have to convert file->nkey and file->keys to the run-time formats.

  // There can be 0 through N keys, and each of those keys has M fields. Each of
  // the M fields has a "unique" flag, which we pass along as an array of INTs.

  int number_of_key_fields = 0;
  for( size_t i=0; i<file->nkey; i++ )
    {
    number_of_key_fields += file->keys[i].nfield;
    }

  // We create an array of pointers for those fields, adding an additional
  // element for a NULL pointer to indicate the end of the list:
  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_keys");
  tree array_of_keys = gg_define_variable(
                                    build_pointer_type(cblc_field_p_type_node),
                                    achName,
                                    scope);
  gg_assign(array_of_keys,
            gg_cast(build_pointer_type(cblc_field_p_type_node),
                    gg_malloc(build_int_cst_type(SIZE_T,
                                                 (number_of_key_fields+1)
                                                 *int_size_in_bytes(VOID_P)))));

  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_keynum");
  tree key_numbers = gg_define_variable(build_pointer_type(INT),
                                        achName,
                                        scope);
  gg_assign(key_numbers,
            gg_cast(build_pointer_type(INT),
                    gg_malloc(build_int_cst_type(SIZE_T,
                                                 (number_of_key_fields+1)
                                                            *int_size_in_bytes(INT)))));

  strcpy(achName, "_");
  strcat(achName, file->name);
  strcat(achName, "_uniqs");
  tree unique_flags = gg_define_variable( build_pointer_type(INT),
                                          achName,
                                          scope);
  gg_assign(unique_flags,
            gg_cast(build_pointer_type(INT),
                    gg_malloc(build_int_cst_type(SIZE_T,
                                                (number_of_key_fields+1)
                                                            *int_size_in_bytes(INT)))));

  size_t index = 0;
  for( size_t i=0; i<file->nkey; i++ )
    {
    for( size_t j=0; j<file->keys[i].nfield; j++ )
      {
      gg_assign(gg_array_value(array_of_keys, index),
                get_field_p(file->keys[i].fields[j]) );

      gg_assign(gg_array_value(key_numbers, index),
                build_int_cst_type(INT, i+1));

      gg_assign(gg_array_value(unique_flags, index),
                (file->keys[i].unique ? integer_one_node : integer_zero_node));
      index += 1;
      }
    }
  // Terminate the field list with a NULL:
  gg_assign( gg_array_value(array_of_keys, index), gg_cast(cblc_field_p_type_node, null_pointer_node) );

  // Terminate the key-numbers list with a negative 1 as a guardrail:
  gg_assign( gg_array_value(key_numbers, index), integer_minusone_node );

  // Terminate the uniques list with a zero, just to avoid garbage:
  gg_assign( gg_array_value(unique_flags, index), integer_zero_node );

  cbl_file_t::varying_t varies = symbol_file_record_sizes(file);

  gcc_assert(varies.min <= varies.max);

  if(file->access == file_inaccessible_e)
    {
    cbl_internal_error(
          "%s:%d file %s access mode is %<file_inaccessible_e%> in %s",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name,
          __func__);
    }

  // This code is a hack needed until the parser sets the varies.min/max
  // properly when they are not equal:
  if(    varies.min != varies.max
      && current_encoding(display_encoding_e) == iconv_UTF16LE_e
      && varies.max == symbol_file_record(file)->data.capacity() )
    {
    fprintf(stderr,
        "There is a hack in genapi.cc to take into account a parser error,\n"
        "namely the fact that when there is a RECORD VARYING clause, the\n"
        "min/max values reflect the values in the source code, while when\n"
        "there is no VARYING clause the min/max values are the same as the\n"
        "default_record's data.capacity().  If you are seeing this message,\n"
        "it would appear the parser has been updated to supply the stride-\n"
        "corrected min/max, and the hack should be removed.\n");
    gcc_assert(false);
    }
  if( varies.max < symbol_file_record(file)->data.capacity())
    {
    const charmap_t *charmap =
                     __gg__get_charmap(current_encoding(display_encoding_e));
    varies.min *= charmap->stride();
    varies.max *= charmap->stride();
    }

  uint64_t symbol_table_index = symbol_unique_index(symbol_elem_of(file));

  gg_call(VOID,
          "__gg__file_init",
          gg_get_address_of(new_var_decl),
          gg_string_literal(file->name),
          build_int_cst_type(UINT64, symbol_table_index),
          array_of_keys,
          key_numbers,
          unique_flags,
          gg_get_address_of(symbol_file_record(file)->var_decl_node),
          get_field_p(file->password),
          get_field_p(file->user_status),
          get_field_p(file->vsam_status),
          get_field_p(file->record_length),
          get_field_p(file_status_register()),
          build_int_cst_type(SIZE_T, file->reserve),
          build_int_cst_type(INT, (int)file->org),
          build_int_cst_type(INT, (int)file->padding),
          build_int_cst_type(INT, (int)file->access),
          build_int_cst_type(INT, (int)file->optional),
          build_int_cst_type(SIZE_T, varies.min),
          build_int_cst_type(SIZE_T, varies.max),
/*  Right now, file->codeset.encoding is not being set properly.  For example,
    when the exec-charset is EBCDIC, file->codeset is coming through as CP1252.
    However, when exec-charset is UTF32LE, file->codeset is arriving here as
    UTF32LE.  Go figure.

    Remove this comment and fix the following code when that's repaired.  */
//          build_int_cst_type(INT, (int)file->codeset.encoding),
          build_int_cst_type(INT, current_encoding(display_encoding_e)),
          build_int_cst_type(INT, (int)file->codeset.alphabet),
          NULL_TREE);
  file->var_decl_node = new_var_decl;
  }

void
parser_file_open( size_t nfiles, struct cbl_file_t *files[], int mode_char )
  {
  for(size_t i=0; i<nfiles; i++)
    {
    auto& file = files[i];
    parser_file_open(file, mode_char);
    }
  }

void
parser_file_open( struct cbl_file_t *file, int mode_char )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      char ach[64];
      sprintf(ach, ", organization is %s", file_org_str(file->org));
      SHOW_PARSE_TEXT(ach);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }

    SHOW_PARSE_TEXT(", mode_char: ")
    char ach[2] = "";
    ach[0] = mode_char;
    SHOW_PARSE_TEXT(ach)

    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("%<parser_file_open%> called with NULL *file");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("%<parser_file_open%> for %s called with NULL "
                       "%<var_decl_node%>", file->name);
    }

  if( mode_char == 'a' && (file->access != file_access_seq_e) )
    {
    cbl_internal_error("EXTEND can only be used where %s is ACCESS MODE SEQUENTIAL", file->name);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_open of ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }


  /*
   * The filename of a cbl_file_t may be found in three places:
   * 1.  As before, in the cbl_field_t indexed by cbl_file_t::filename.
   * 2.  Now, in the cbl_special_name_t indexed by cbl_file_t::device.
   * 3.  As ever, in neither, from the environment.
   *
   * If both filename and device are nonzero and not FldForward, the filename
   * supersedes. The syntax was
   *     SELECT fd-name ASSIGN TO device-name USING filename
   * That just creates in the parser an alias of device-name to fd-name.  It's
   * still the same file and does *not* change the device characteristics.
   *
   * If filename is FldForward (or 0) and device is nonzero, the OS filename is
   * taken from cbl_special_name_t::os_filename.  It is tiny, hard-coded name
   * in /dev.
   *
   * --jkl
   */

  tree pszFilename = gg_define_variable(CHAR_P);
  cbl_field_t *field_of_name = symbol_field_forward(file->filename);
  if( field_of_name->type == FldForward )
    {
    // The target of ASSIGN TO was unquoted, but didn't resolve to a
    // cbl_field_t.  This means that the name of the field is an
    // environment variable that will hold the file name
    gg_assign(pszFilename, gg_strdup(gg_string_literal(field_of_name->name)));
    }
  else
    {
    gg_assign(pszFilename, gg_cast(CHAR_P, null_pointer_node));
    }

  sv_is_i_o = true;
  store_location_stuff("OPEN");
  gg_call(VOID,
          "__gg__file_open",
          gg_get_address_of(file->var_decl_node),
          field_of_name->var_decl_node
                  ? gg_get_address_of(field_of_name->var_decl_node)
                  : null_pointer_node,
          pszFilename,
          build_int_cst_type(INT, mode_char),
          NULL_TREE);
  set_user_status(file);
  }

void
parser_file_close( struct cbl_file_t *file, file_close_how_t how )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL ")
      }
    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("%<parser_file_close%> called with NULL *file");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("%<parser_file_close%> for %s called with "
                       "NULL %<file->var_decl_node%>", file->name);
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_close of ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }

  // We are done with the filename.  The library routine will free "filename"
  // memory and set it back to null

  sv_is_i_o = true;
  store_location_stuff("CLOSE");
  gg_call(VOID,
          "__gg__file_close",
          gg_get_address_of(file->var_decl_node),
          build_int_cst_type(INT, (int)how),
          NULL_TREE);
  set_user_status(file);
  }

void
parser_file_read( struct cbl_file_t *file,
                  cbl_refer_t /*data_dest*/,
                  int where )
  {
  Analyze();
  // where = -2 means PREVIOUS
  // where = -1 means NEXT
  // where =  1 or more means key N, where N is one-based
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }

    char ach[32];
    sprintf(ach, " where:%d", where);
    SHOW_PARSE_TEXT(ach)
    SHOW_PARSE_END
    }

  if( where == 0 )
    {
    cbl_internal_error("%s:%d file %s 'where' is zero in %s",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name,
          __func__);
    where = -1;
    }

  if( !file )
    {
    cbl_internal_error("%<parser_file_read%> called with NULL *file");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("%<parser_file_read%> for %s called with "
                       "NULL %<file->var_decl_node%>", file->name);
    }

  if( !file )
    {
    cbl_internal_error("%<parser_file_read%> called with NULL *field");
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("%<parser_file_read%> for %s called with "
                       "NULL %<field->var_decl_node%>", file->name);
    }

  if( file->access == file_access_seq_e && where >= 0)
    {
    cbl_internal_error("%s:%d file %s is RELATIVE/SEQUENTIAL, but %<where >= 0%>",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name);
    where = -1;
    }

  if( file->access == file_access_rnd_e && where < 0)
    {
    cbl_internal_error("%s:%d file %s is RELATIVE/RANDOM, but %<where < 0%>",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name);
    where = 1;
    }

  sv_is_i_o = true;
  store_location_stuff("READ");
  gg_call(VOID,
          "__gg__file_read",
          gg_get_address_of(file->var_decl_node),
          build_int_cst_type(INT, where),
          NULL_TREE);
  set_user_status(file);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("from ")
    TRACE1_TEXT(file->name);
    TRACE1_INDENT
    cbl_field_t *our_return_code
                  = cbl_field_of(symbol_at(file_status_register()));
    TRACE1_FIELD("result: ", our_return_code, "");
    TRACE1_END
    }
  }

void
parser_file_write( cbl_file_t *file,
                   cbl_field_t *record_area,
                   bool after,
                   cbl_refer_t &advance,
                   bool sequentially
                 )
  {
  Analyze();

  bool is_random = !(   file->access == file_access_seq_e
                     || file->access == file_inaccessible_e);

  if( (is_random ? 1 : 0) != (sequentially ? 0 : 1) )
    {
    cbl_internal_error("%s:%d file %s 'sequentially' is %d in %s",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name,
          sequentially ? 1 : 0,
          __func__);
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }

    if( !advance.field )
      {
      SHOW_PARSE_TEXT(" automatic BEFORE ADVANCING 1 LINE")
      }
    else
      {
      if( after )
        {
        SHOW_PARSE_TEXT(" AFTER")
        }
      else
        {
        SHOW_PARSE_TEXT(" BEFORE")
        }
      SHOW_PARSE_REF(" ADVANCING ", advance);
      SHOW_PARSE_TEXT(" LINE(S)")
      }

    SHOW_PARSE_END
    }

  if( !file )
    {
    cbl_internal_error("%s: called with NULL *file", __func__);
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error("%s: for %s called with NULL %<file->var_decl_node%>",
                        __func__, file->name);
    }

  if( !file )
    {
    cbl_internal_error("%s: called with NULL *field", __func__);
    }

  if( !file->var_decl_node )
    {
    cbl_internal_error( "%s: for %s called with NULL %<field->var_decl_node%>",
                        __func__,
                        file->name);
    }

  tree t_advance = gg_define_variable(INT);
  if(advance.field)
    {
    tree value;
    get_binary_value( value, advance, INT);
    gg_assign(t_advance, gg_cast(INT, value));
    }
  else
    {
    if( file->org == file_line_sequential_e )
      {
      // ISO/IEC_1989-2014 and IBM say the default is AFTER advancing
      // MicroFocus and GnuCOBOL say the default is BEFORE advancing.
      // See the comment where the variable is defined:
      after = auto_advance_is_AFTER_advancing;
      gg_assign(t_advance, integer_one_node);
      }
    else
      {
      // The default for SEQUENTIAL is no vertical motion
      gg_assign(t_advance, integer_minusone_node);
      }
    }

  gcc_assert(record_area);
  if( !record_area )
    {
    record_area = cbl_field_of(symbol_at(file->default_record));
    }

  sv_is_i_o = true;
  store_location_stuff("WRITE");
  gg_call(VOID,
          "__gg__file_write",
          gg_get_address_of(file->var_decl_node),
          member(record_area, "data"),
          member(record_area, "capacity"),
          after ? integer_one_node : integer_zero_node,
          t_advance,
          is_random ? integer_one_node : integer_zero_node,
          NULL_TREE);
  set_user_status(file);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("to ")
    TRACE1_TEXT(file->name);
    TRACE1_INDENT
    if( advance.field )
      {
      TRACE1_INDENT
      if( after )
        {
        TRACE1_TEXT("AFTER")
        }
      else
        {
        TRACE1_TEXT("BEFORE")
        }
      TRACE1_REFER(" ADVANCING ", advance, " LINE(S)");
      }
    TRACE1_INDENT
    cbl_field_t *our_return_code
                  = cbl_field_of(symbol_at(file_status_register()));
    TRACE1_FIELD("result: ", our_return_code, "");
    TRACE1_END
    }
  }

void
parser_file_delete( struct cbl_file_t *file, bool /*sequentially*/ )
  {
  Analyze();

  if( !file )
    {
    cbl_internal_error("The file pointer should not be null");
    abort();  // Because cppcheck doesn't recognize [[noerror]]
    }

  bool sequentially =    file->access == file_access_seq_e
                      || file->org    == file_sequential_e
                      || file->org    == file_line_sequential_e;
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      if( sequentially )
        {
        SHOW_PARSE_TEXT(" sequentially")
        }
      else
        {
        SHOW_PARSE_TEXT(" sequentially")
        }
      }
    SHOW_PARSE_END
    }

  sv_is_i_o = true;
  store_location_stuff("DELETE");
  gg_call(VOID,
          "__gg__file_delete",
          gg_get_address_of(file->var_decl_node),
          sequentially ? integer_zero_node : integer_one_node,
          NULL_TREE);
  set_user_status(file);

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("parser_file_delete record ")
    TRACE1_TEXT(file->name);
    TRACE1_END
    }
  }

static void
set_up_delete_file_label(cbl_label_t *delete_file_label)
  {
  if( delete_file_label )
    {
    if( !delete_file_label->structs.delete_file )
      {
      delete_file_label->structs.delete_file
        = static_cast<cbl_delete_file_t *>
                                  (xmalloc(sizeof(struct cbl_delete_file_t)));
      // Set up the address pairs for this clause
      gg_create_goto_pair(
                  &delete_file_label->structs.delete_file->over.go_to,
                  &delete_file_label->structs.delete_file->over.label);
      gg_create_goto_pair(
                  &delete_file_label->structs.delete_file->exception.go_to,
                  &delete_file_label->structs.delete_file->exception.label);
      gg_create_goto_pair(
                  &delete_file_label->structs.delete_file->no_exception.go_to,
                  &delete_file_label->structs.delete_file->no_exception.label);
      gg_create_goto_pair(
                  &delete_file_label->structs.delete_file->bottom.go_to,
                  &delete_file_label->structs.delete_file->bottom.label);
      }
    }
  }

void
parser_file_delete_file( cbl_label_t *name,
                         std::vector<cbl_file_t*> filenames )
  {
  // This removes a file from the file system.  It is distinct from the
  // FILE DELETE statement, which deletes a record from a file.
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ");
    SHOW_PARSE_TEXT(name->name);
    for(size_t i=0; i<filenames.size(); i++)
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT(filenames[i]->name)
      }
    SHOW_PARSE_END
    }
  set_up_delete_file_label(name);
  tree there_was_an_error = gg_define_variable(INT, 0L);
  for(size_t i=0; i<filenames.size(); i++)
    {
    tree pszFilename = gg_define_variable(CHAR_P);
    cbl_field_t *field_of_name = symbol_field_forward(filenames[i]->filename);
    if( field_of_name->type == FldForward )
      {
      // The target of ASSIGN TO was unquoted, but didn't resolve to a
      // cbl_field_t.  This means that the name of the field is an
      // environment variable that will hold the file name
      gg_assign(pszFilename,
                gg_strdup(gg_string_literal(field_of_name->name)));
      }
    else
      {
      gg_assign(pszFilename, gg_cast(CHAR_P, null_pointer_node));
      }
    gg_assign(there_was_an_error,
              gg_bitwise_or(there_was_an_error,
                            gg_call_expr(
                            INT,
                            "__gg__file_remove",
                            gg_get_address_of(filenames[i]->var_decl_node),
                            field_of_name->var_decl_node
                              ? gg_get_address_of(field_of_name->var_decl_node)
                              : null_pointer_node,
                            pszFilename,
                            NULL_TREE)));
    set_user_status(filenames[i]);
    }
  IF( there_was_an_error, eq_op, integer_zero_node )
    {
    // There was no error detected.
    gg_append_statement(name->structs.delete_file->no_exception.go_to);
    }
  ELSE
    {
    // There was an error detected.
    gg_append_statement(name->structs.delete_file->exception.go_to);
    }
  }

void
parser_file_delete_on_exception( cbl_label_t *name )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ");
    SHOW_PARSE_TEXT(name->name);
    SHOW_PARSE_END
    }
  gg_append_statement(name->structs.delete_file->bottom.go_to);
  gg_append_statement(name->structs.delete_file->exception.label);
  }

void
parser_file_delete_not_exception( cbl_label_t *name )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ");
    SHOW_PARSE_TEXT(name->name);
    SHOW_PARSE_END
    }
  gg_append_statement(name->structs.delete_file->bottom.go_to);
  gg_append_statement(name->structs.delete_file->no_exception.label);
  }

void
parser_file_delete_end( cbl_label_t *name )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ");
    SHOW_PARSE_TEXT(name->name);
    SHOW_PARSE_END
    }
  gg_append_statement(name->structs.delete_file->bottom.label);
  }

void
parser_file_rewrite(cbl_file_t *file,
                    cbl_field_t *record_area,
                    bool sequentially )
  {
  Analyze();
  if(    file->org    == file_indexed_e
      && file->access == file_access_seq_e
      && !sequentially )
    {
    cbl_internal_error(
          "%s:%d file %s is INDEXED/SEQUENTIAL, but 'sequentially' is false",
          current_filename.back().c_str(),
          CURRENT_LINE_NUMBER,
          file->name);
    sequentially = true;
    }

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }
    SHOW_PARSE_END
    }

  gcc_assert(record_area);
  if( !record_area )
    {
    record_area = cbl_field_of(symbol_at(file->default_record));
    }

  sv_is_i_o = true;
  store_location_stuff("REWRITE");
  gg_call(VOID,
          "__gg__file_rewrite",
          gg_get_address_of(file->var_decl_node),
          member(record_area, "capacity"),
          sequentially ? integer_zero_node : integer_one_node,
          NULL_TREE);
  set_user_status(file);
  }

/*
 * flk is first-last-key.  Similar to parser_file_read, it is a
 * 1-based index, for consistency.  Encoded values:
 *   -1 FIRST
 *   -2 LAST
 *    0 neither
 *   >0 1-based index into cbl_file_t::keys
 */
void
parser_file_start(struct cbl_file_t *file,
                  relop_t op,
                  int flk,
            const cbl_refer_t &length_ref )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      switch(op)
        {
        case lt_op:
          SHOW_PARSE_TEXT(" lt_op")
          break;
        case le_op:
          SHOW_PARSE_TEXT(" le_op")
          break;
        case eq_op:
          SHOW_PARSE_TEXT(" eq_op")
          break;
        case ne_op:
          SHOW_PARSE_TEXT(" ne_op")
          break;
        case ge_op:
          SHOW_PARSE_TEXT(" ge_op")
          break;
        case gt_op:
          SHOW_PARSE_TEXT(" gt_op")
          break;
        }
      char ach[32];
      sprintf(ach, " first-last-key:%d", flk);
      SHOW_PARSE_TEXT(ach)
      SHOW_PARSE_REF(" length:", length_ref);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL")
      }
    SHOW_PARSE_END
    }

  if(     flk == 0
      &&  (file->org == file_indexed_e || file->org == file_relative_e) )
    {
    flk = 1;
    op = eq_op;
    }

  if(     flk == 0
      &&  (file->org == file_sequential_e) )
    {
    flk = -1;
    }

  tree length = size_t_zero_node;

  if( flk > 0 && !length_ref.field )
    {
    // We need a length, and we don't have one.  We have to calculate the
    // length from the lengths of the fields that make up the specified key.

    size_t combined_length = 0;

    gcc_assert(flk <= (int)file->nkey);

    int key_number = flk-1;

    // A key has a number of fields
    for(size_t ifield=0; ifield<file->keys[key_number].nfield; ifield++)
      {
      size_t nfield = file->keys[key_number].fields[ifield];
      cbl_field_t *field = cbl_field_of(symbol_at(nfield));
      combined_length += field->data.capacity();
      }
    length = build_int_cst_type(SIZE_T, combined_length);
    }
  else if( flk > 0 )
    {
    get_binary_value( length, length_ref, SIZE_T);
    }

  sv_is_i_o = true;
  store_location_stuff("START");
  gg_call(VOID,
          "__gg__file_start",
          gg_get_address_of(file->var_decl_node),
          build_int_cst_type(INT, op),
          build_int_cst_type(INT, flk),
          length,
          NULL_TREE );
  set_user_status(file);
  }

static void
inspect_tally(bool backward,
        const cbl_refer_t &identifier_1,
              cbl_inspect_opers_t& identifier_2)
  {
  Analyze();
  // This is an INSPECT FORMAT 1
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    char ach[128];
    sprintf(ach, "There are %lu identifier_2", gb4(identifier_2.size()));
    SHOW_PARSE_TEXT(ach);
    for(size_t i=0; i<identifier_2.size(); i++)
      {
      SHOW_PARSE_INDENT
        sprintf(ach, "%lu: bounds: %lu", gb4(i), gb4(identifier_2[i].nbound()));
      SHOW_PARSE_TEXT(ach);
      for(size_t j=0; j<identifier_2[i].nbound(); j++)
        {
        SHOW_PARSE_INDENT
          sprintf(ach, "    %lu: matches: %lu",
                  gb4(j), gb4(identifier_2[i][j].matches.size()));
        SHOW_PARSE_TEXT(ach);

        SHOW_PARSE_INDENT
        if(  identifier_2[i][j].bound == bound_characters_e )
          {
          SHOW_PARSE_TEXT("       bound_characters");
          }
        else
          {
          SHOW_PARSE_TEXT("       bound_leading/all");
          }

        if( identifier_2[i][j].matches.size() )
          {
          SHOW_PARSE_INDENT
          sprintf(ach, "       before %p",
                  as_voidp(identifier_2.at(i).at(j).matches.at(0).before.identifier_4.field));
          SHOW_PARSE_TEXT(ach);
          SHOW_PARSE_INDENT
          sprintf(ach, "       after  %p",
                  as_voidp(identifier_2.at(i).at(j).matches.at(0).after.identifier_4.field));
          SHOW_PARSE_TEXT(ach);
          }
        }
      }

    SHOW_PARSE_END
    }

  // Make one pass through the inputs to count up the sizes of the arrays
  // we will be passing to the library routines.  This loop structure simply
  // anticipates the more complex one that follows.

  size_t int_index  = 0;
  size_t pcbl_index = 0;
  unsigned long n_identifier_2 = identifier_2.size();

  // The first integer is the all-important controlling count:
  int_index++;

  // The first refer is for identifier-1
  pcbl_index++;

  for( size_t i=0; i<n_identifier_2; i++)
    {
    // Each identifier-2 has to go into the array:
    pcbl_index++;
    // For each FOR there is a count of the loops after the FOR
    int_index++;
    for(size_t j=0; j<identifier_2[i].nbound(); j++)
      {
      // After each identifier-2, there is a cbl_inspect_bound_t value:
      int_index++;
      if( identifier_2[i][j].bound == bound_characters_e)
        {
        // This is a FOR CHARACTERS PHRASE1, so we will need before/after
        // for each:
        pcbl_index++;
        pcbl_index++;
        }
      else
        {
        // This is ALL or LEADING.  Each has some number of identifier-3
        int_index++;
        for(size_t k=0; k<identifier_2[i][j].n_identifier_3(); k++)
          {
          // Put identifier-3 into the array:
          pcbl_index++;

          // We need the PHRASE1 for that identifier-3
          pcbl_index++;
          pcbl_index++;
          }
        }
      }
    }

  // We will be passing the library routine an array of size_t, which contains
  // all the integers and cbl_inspect_bound_t values, in a strict sequence so
  // that the library routine can peel them off.

  tree int_size = gg_define_variable(INT, 0L);
  tree integers = gg_define_variable(SIZE_T_P, null_pointer_node);

  size_t n_integers = int_index;

  IF( build_int_cst_type(INT, n_integers), gt_op, int_size )
    {
    gg_assign(int_size, build_int_cst_type(INT, n_integers));
    gg_assign(integers,
              gg_cast(SIZE_T_P,
                      gg_realloc(integers,
                                 n_integers
                                 * int_size_in_bytes(VOID_P))));
    }
  ELSE
    {
    }
  ENDIF

  const size_t n_resolveds = pcbl_index;
  std::vector<cbl_refer_t> pcbl_refers(n_resolveds);

  // Now we make a second pass, populating those arrays:
  int_index  = 0;
  pcbl_index = 0;

  // The first integer is the all-important controlling count:
  gg_assign(  gg_array_value(integers, int_index++),
              build_int_cst_type(SIZE_T, n_identifier_2) );

  // The first refer is for identifier-1
  pcbl_refers[pcbl_index++] = identifier_1;

  for( size_t i=0; i<n_identifier_2; i++)
    {
    // Each identifier-2 has to go into the array:
    pcbl_refers[pcbl_index++] = identifier_2[i].tally;
    // For each FOR there is a count of the loops after the FOR
    gg_assign(  gg_array_value(integers, int_index++),
                build_int_cst_type(SIZE_T, identifier_2[i].nbound()) );
    for(size_t j=0; j<identifier_2[i].nbound(); j++)
      {

      // After each identifier-2, there is a cbl_inspect_bound_t value:
      gg_assign(  gg_array_value(integers, int_index++),
                  build_int_cst_type(SIZE_T, identifier_2[i][j].bound));
      if( identifier_2[i][j].bound == bound_characters_e)
        {
        // This is a FOR CHARACTERS PHRASE1, so we will need before/after
        // for each:
        const auto& m( identifier_2[i][j].matches );
        if( m.empty() )
          {
            pcbl_index += 2;
          }
        else
          {
          pcbl_refers[pcbl_index++] = m[0].before.identifier_4;
          pcbl_refers[pcbl_index++] = m[0].after.identifier_4;
          }
        }
      else
        {
        // This is ALL or LEADING.  Each has some number of identifier-3
        gg_assign(  gg_array_value(integers, int_index++),
                    build_int_cst_type(SIZE_T, identifier_2[i][j].n_identifier_3()));
        for(size_t k=0; k<identifier_2[i][j].n_identifier_3(); k++)
          {
          // Put identifier-3 into the array:
          pcbl_refers[pcbl_index++] = identifier_2[i][j].matches[k].matching();

          // We need the PHRASE1 for that identifier-3
          pcbl_refers[pcbl_index++] = identifier_2[i][j].matches[k].before.identifier_4;

          pcbl_refers[pcbl_index++] = identifier_2[i][j].matches[k].after.identifier_4;
          }
        }
      }
    }

  //fprintf(stderr, " %ld %ld\n", int_index, n_integers);
  gcc_assert(int_index  == n_integers);
  //fprintf(stderr, " %ld %ld\n", pcbl_index, n_resolveds);
  gcc_assert(pcbl_index == n_resolveds);

  // We have built up an array of integers, and an array of cbl_refer_t.
  tree params = build_array_of_referlets(pcbl_index, pcbl_refers.data());

  // Do the actual call:
  charmap_t *charmap = __gg__get_charmap(identifier_1.field->codeset.encoding);
  if( charmap->stride() == 1 && !charmap->is_like_utf8() )
    {
    // The variables are ASCII or EBCDIC
    gg_call(VOID,
            "__gg__inspect_format_1_sbc",
            backward ? integer_one_node : integer_zero_node,
            integers,
            params,
            NULL_TREE);
    }
  else
    {
    gg_call(VOID,
            "__gg__inspect_format_1",
            backward ? integer_one_node : integer_zero_node,
            integers,
            params,
            NULL_TREE);
    }
  }

static void
inspect_replacing(int backward,
            const cbl_refer_t &identifier_1,
                  cbl_inspect_opers_t &operations)
  {
  Analyze();
  // This is an INSPECT FORMAT 2
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    }

  // For REPLACING, unlike TALLY, there can be but one operation
  unsigned long n_ops = operations.size();
  gcc_assert(n_ops == 1);

  size_t n_id_3 = 0;
  size_t n_id_4 = 0;
  size_t n_id_5 = 0;
  size_t n_all_leading_first = 0;

  // Make one pass through the inputs to count up the sizes of the arrays
  // we will be passing to the library routines:

  for( size_t j=0; j<operations[0].nbound(); j++)
    {
    if( operations[0][j].bound == bound_characters_e)
      {
      // This is a FOR CHARACTERS phrase

      // Each will have an identifier-5:
      n_id_5 += 1;

      // Each will have a PHRASE1 comprising BEFORE and AFTER identifier-4 values
      n_id_4 += 2;
      }
    else
      {
      // This is ALL, LEADING, or FIRST.  Each has some number of identifier-3 values:
      // The n_identifier_3 value goes into the integer list, so we'll have
      // to make room for them:
      n_all_leading_first += 1;

      // The n_identifier-3 values will go into the resolved values; we have to
      // leave room for them
      n_id_3 += operations[0][j].n_identifier_3();

      // Likewise identifier-5 values:
      n_id_5 += operations[0][j].n_identifier_3();

      // And each identifier-3 / identifier-5 pair has BEFORE and AFTER phrases:
      n_id_4 += 2 * operations[0][j].n_identifier_3();
      }
    }

  // We will be passing the library routine an array of size_t, which contains
  // all the integers and cbl_inspect_bound_t values, in a strict sequence so
  // that the library routine can peel them off.

  size_t n_integers =   1                     // Room for operations[0].nbound()
                        + operations[0].nbound()  // Room for all the cbl_inspect_bound_t values
                        + n_all_leading_first;  // Room for all of the  n_identifier_3  counts

  tree int_size = gg_define_variable(INT, 0L);
  tree integers = gg_define_variable(SIZE_T_P, null_pointer_node);

  IF( build_int_cst_type(INT, n_integers), gt_op, int_size )
    {
    gg_assign(int_size, build_int_cst_type(INT, n_integers));
    gg_assign(integers,
              gg_cast(SIZE_T_P,
                      gg_realloc(integers,
                                 n_integers
                                 * int_size_in_bytes(VOID_P))));
    }
  ELSE
    {
    }
  ENDIF

  const size_t n_resolveds =  1                 // Room for identifier-1
                            + n_id_3            // Room for the identifier-3 variables
                            + n_id_4            // Room for the identifier-4 variables
                            + n_id_5;           // Room for the identifier-5 variables

  std::vector<cbl_refer_t> pcbl_refers(n_resolveds);

  // Now we make a second pass, populating those arrays:
  size_t int_index  = 0;
  size_t pcbl_index = 0;

  // The first integer is the all-important controlling count:
  gg_assign(  gg_array_value(integers, int_index++),
              build_int_cst_type(SIZE_T, operations[0].nbound()) );

  // The first refer is for identifier-1
  pcbl_refers[pcbl_index++] = identifier_1;

  for( size_t j=0; j<operations[0].nbound(); j++)
    {
    // For each FOR there is a count of the loops after the FOR

    // For each operation, there is a cbl_inspect_bound_t value:
    gg_assign(  gg_array_value(integers, int_index++),
                build_int_cst_type(SIZE_T, operations[0][j].bound));
    if( operations[0][j].bound == bound_characters_e)
      {
      // This is a FOR CHARACTERS PHRASE1

      // Put in the identifier-5 replacement value:
      pcbl_refers[pcbl_index++] = operations[0][j].replaces[0].replacement;

      // Each identifier-5 gets a PHRASE1:
      pcbl_refers[pcbl_index++] = operations[0][j].replaces[0].before.identifier_4;
      pcbl_refers[pcbl_index++] = operations[0][j].replaces[0].after.identifier_4;

      SHOW_PARSE
        {
        if( j )
          {
          SHOW_PARSE_INDENT
          }
        SHOW_PARSE_FIELD("ID-5 ", operations[0][j].replaces[0].replacement.field)
        if(operations[0][j].replaces[0].before.identifier_4.field)
          {
          SHOW_PARSE_FIELD(" before ", operations[0][j].replaces[0].before.identifier_4.field)
          }
        if(operations[0][j].replaces[0].after.identifier_4.field)
          {
          SHOW_PARSE_FIELD(" after ", operations[0][j].replaces[0].after.identifier_4.field)
          }
        SHOW_PARSE_END
        }
      }
    else
      {
      // This is ALL or LEADING.  Each has some number of identifier-3/identifier-5 pairs
      gg_assign(  gg_array_value(integers, int_index++),
                  build_int_cst_type(SIZE_T, operations[0][j].n_identifier_3()));
      for(size_t k=0; k<operations[0][j].n_identifier_3(); k++)
        {
        // Put identifier-3 into the array:
        pcbl_refers[pcbl_index++] = operations[0][j].replaces[k].matching();

        // Put in the identifier-5 replacement value:
        pcbl_refers[pcbl_index++] = operations[0][j].replaces[k].replacement;

        // We need the PHRASE1 for that identifier-3/identifier-5 pair:
        pcbl_refers[pcbl_index++] = operations[0][j].replaces[k].before.identifier_4;

        pcbl_refers[pcbl_index++] = operations[0][j].replaces[k].after.identifier_4;

        SHOW_PARSE
          {
          if( j || k )
            {
            SHOW_PARSE_INDENT
            }
          SHOW_PARSE_FIELD("ID-3 ", operations[0][j].replaces[k].matching().field)
          SHOW_PARSE_FIELD(" ID-5 ", operations[0][j].replaces[k].replacement.field)
          if( operations[0][j].replaces[k].before.identifier_4.field )
            {
            SHOW_PARSE_FIELD("before ", operations[0][j].replaces[k].before.identifier_4.field)
            }
          if(operations[0][j].replaces[k].after.identifier_4.field)
            {
            SHOW_PARSE_FIELD("after ", operations[0][j].replaces[k].after.identifier_4.field)
            }
          SHOW_PARSE_END
          }
        }
      }
    }

  //fprintf(stderr, "%s: %ld %ld\n", __func__, int_index, n_integers);
  gcc_assert(int_index  == n_integers);
  //fprintf(stderr, "%s: %ld %ld\n", __func__, pcbl_index, n_resolveds);
  gcc_assert(pcbl_index == n_resolveds);

  // We have built up an array of integers, and an array of cbl_refer_t.

  for(size_t i=0; i<pcbl_index; i++)
    {
    if( pcbl_refers[i].field && pcbl_refers[i].field->type == FldLiteralN )
      {
      fprintf(stderr, "INSPECT field %s shouldn't be a FldLiteralN\n",
              pcbl_refers[i].field->name);
      gcc_unreachable();
      }
    }

  tree params = build_array_of_referlets(pcbl_index, pcbl_refers.data());

  // Do the actual call:
  gg_call(VOID,
          "__gg__inspect_format_2",
          backward ? integer_one_node : integer_zero_node,
          integers,
          params,
          NULL_TREE);
  }

void
parser_inspect(const cbl_refer_t& identifier_1,
               bool backward,
               cbl_inspect_opers_t& operations)
  {
  Analyze();
  gcc_assert(! operations.empty());

  /*  Operating philosophy:  We are going to minimize the amount of
      GENERIC tag creation here at compile time, mainly by eliminating
      the generation of cbl_resolved_t structures that we know
      contain no information. */

  if( operations[0].tally.field )
    {
    // This is a FORMAT 1 "TALLYING"
    inspect_tally(backward, identifier_1, operations);
    }
  else
    {
    // This is a FORMAT 2 "REPLACING"
    inspect_replacing(backward, identifier_1, operations);
    }
  }

void
parser_inspect_conv(cbl_refer_t input,
                    bool backward,
                    cbl_refer_t original,
                    cbl_refer_t replacement,
                    cbl_inspect_qual_t before,
                    cbl_inspect_qual_t after )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  gg_call(CHAR_P,
          "__gg__inspect_format_4",
          backward ? integer_one_node : integer_zero_node,
          input.field ? gg_get_address_of(input.field->var_decl_node)
                      : null_pointer_node,
          refer_offset(input),
          refer_size_source(input),
          original.field ? gg_get_address_of(original.field->var_decl_node)
                         : null_pointer_node,
          refer_offset(original),
          refer_size_dest(original),
          replacement.field ? gg_get_address_of(
                              replacement.field->var_decl_node)
                            : null_pointer_node,
          refer_offset(replacement),
          replacement.all ? build_int_cst_type(SIZE_T, -1LL)
                          : refer_size_source(replacement),
          after.identifier_4.field ? gg_get_address_of(
                                        after.identifier_4.field->var_decl_node)
                                   : null_pointer_node,
          refer_offset(after.identifier_4),
          refer_size_source(after.identifier_4),
          before.identifier_4.field ? gg_get_address_of(
                                       before.identifier_4.field->var_decl_node)
                                    : null_pointer_node,
          refer_offset(before.identifier_4),
          refer_size_source(before.identifier_4),
          NULL_TREE
          );
  }

void
parser_intrinsic_find_string(cbl_field_t *tgt,
                             const cbl_refer_t& haystack,
                             const cbl_refer_t& needle,
                             const cbl_refer_t *after,
                             bool last,
                             bool anycase)
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  gg_call(VOID,
          "__gg__find_string",
          gg_get_address_of( tgt->var_decl_node),
          gg_get_address_of( haystack.field->var_decl_node),
          refer_offset(      haystack),
          refer_size_source( haystack),
          gg_get_address_of( needle.field->var_decl_node),
          refer_offset(      needle),
          refer_size_source( needle),
          after ? gg_get_address_of( after->field->var_decl_node)
                : null_pointer_node,
          after ? refer_offset(*after) : size_t_zero_node,
          after ? refer_size_source(*after) : size_t_zero_node,
          last   ?integer_one_node:integer_zero_node,
          anycase?integer_one_node:integer_zero_node,
          NULL_TREE
          );
  }

void
parser_intrinsic_convert(cbl_field_t *tgt,
                         const cbl_refer_t& input,
                         convert_type_t src_fmt,
                         unsigned int   dst_fmt )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  gg_call(VOID,
          "__gg__convert",
          gg_get_address_of(tgt->var_decl_node),
          gg_get_address_of(input.field->var_decl_node),
          refer_offset(input),
          refer_size_source(input),
          build_int_cst_type(INT, src_fmt),
          build_int_cst_type(INT, dst_fmt),
          NULL_TREE);
  }

void
parser_module_name( cbl_field_t *tgt, module_type_t type )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  gg_call(VOID,
          "__gg__module_name",
          gg_get_address_of(tgt->var_decl_node),
          build_int_cst_type(INT, type),
          NULL_TREE);
  }

void
parser_intrinsic_numval_c( cbl_field_t *f,
                           cbl_refer_t& input,
                           bool locale,
                           cbl_refer_t& currency,
                           bool anycase,
                           bool test_numval_c ) // true for TEST-NUMVAL-C
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }
  if( locale || anycase )
    {
    gcc_unreachable();
    }
  if( test_numval_c )
    {
    gg_call(INT,
            "__gg__test_numval_c",
            gg_get_address_of(f->var_decl_node),
            gg_get_address_of(input.field->var_decl_node),
            refer_offset(input),
            refer_size_source(input),
            currency.field ? gg_get_address_of(currency.field->var_decl_node) : null_pointer_node,
            refer_offset(currency),
            refer_size_source(currency),
            NULL_TREE
            );
    }
  else
    {
    gg_call(INT,
            "__gg__numval_c",
            gg_get_address_of(f->var_decl_node),
            gg_get_address_of(input.field->var_decl_node),
            refer_offset(input),
            refer_size_source(input),
            currency.field ? gg_get_address_of(currency.field->var_decl_node) : null_pointer_node,
            refer_offset(currency),
            refer_size_source(currency),
            NULL_TREE
            );
    }
  }

void
parser_intrinsic_subst( cbl_field_t *f,
                  const cbl_refer_t& ref1,
                        size_t argc,
                        cbl_substitute_t * argv )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" TO ", f)
    for(size_t i=0; i<argc; i++)
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_FIELD(" ", argv[i].orig.field)
      SHOW_PARSE_FIELD(" ", argv[i].replacement.field)
      }
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  sv_is_i_o = true;
  store_location_stuff("SUBSTITUTE");
  unsigned char *control_bytes =
           static_cast<unsigned char *>(xmalloc(argc * sizeof(unsigned char)));
  gcc_assert(control_bytes);
  std::vector<cbl_refer_t> arg1(argc);
  std::vector<cbl_refer_t> arg2(argc);

  for(size_t i=0; i<argc; i++)
    {
    control_bytes[i] =   (argv[i].anycase ?
                                  substitute_anycase_e : 0)
                       + (argv[i].first_last == cbl_substitute_t::subst_first_e ?
                                  substitute_first_e : 0)
                       + (argv[i].first_last == cbl_substitute_t::subst_last_e ?
                                  substitute_last_e : 0);
    arg1[i] = argv[i].orig;
    arg2[i] = argv[i].replacement;
    }

  tree control = gg_array_of_bytes(argc, control_bytes);

  tree ref_arg1 = build_array_of_referlets(argc, arg1.data());
  tree ref_arg2 = build_array_of_referlets(argc, arg2.data());

  gg_call(VOID,
          "__gg__substitute",
          ref_arg1,
          ref_arg2,
          gg_get_address_of(f->var_decl_node),
          gg_get_address_of(ref1.field->var_decl_node),
          refer_offset(ref1),
          refer_size_source(ref1),
          build_int_cst_type(SIZE_T, argc),
          control,
          NULL_TREE);

  gg_free(control);

  free(control_bytes);
  }

void
parser_intrinsic_callv( cbl_field_t *tgt,
                        const char function_name[],
                        size_t nrefs,
                        cbl_refer_t *refs )
  {
  Analyze();
  // We have been given an array of refs[nrefs].  Each ref is a pointer
  // to a cbl_ref_t.  We convert that to a table of pointers to run-time
  // cblc_ref_t structures, and we pass that to the function_name intrinsic
  // function.  It is in charge of conversion to whatever form is needed.

  // We get back a return value, which we convert to tgt based on the
  // intrinsic_return_type

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    fprintf(stderr, " with " HOST_SIZE_T_PRINT_DEC " parameters",
            (fmt_size_t)nrefs);
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    for(size_t i=0; i<nrefs; i++)
      {
      TRACE1_INDENT
      gg_fprintf(trace_handle, 1, "parameter %ld: ", build_int_cst_type(SIZE_T, i+1));
      TRACE1_REFER("", refs[i], "")
      }
    }
  store_location_stuff(function_name);
  tree ncount = build_int_cst_type(SIZE_T, nrefs);

  tree refers = build_array_of_refers(nrefs, refs);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          ncount,
          refers,
          NULL_TREE);

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_0(cbl_field_t *tgt,
                        const char function_name[])
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    }

  if( strcmp(function_name, "__gg__random") == 0 )
    {
    // We have no seed value, so call the "next" routine
    gg_call(VOID,
            "__gg__random_next",
            gg_get_address_of(tgt->var_decl_node),
            NULL_TREE);
    }
  else if( strcmp(function_name, "__gg__when_compiled") == 0 )
    {
    // Pass __gg__when_compiled() the time from right now.
    struct timespec tp;
    uint64_t now = get_time_nanoseconds();
    tp.tv_sec  = now / 1000000000;
    tp.tv_nsec = now % 1000000000;

    store_location_stuff(function_name);
    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            build_int_cst(SIZE_T, tp.tv_sec),
            build_int_cst(LONG,   tp.tv_nsec),
            NULL_TREE);
    }
  else
    {
    store_location_stuff(function_name);
    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            NULL_TREE);
    }

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_1( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  // There are special cases:
  if( strstr(function_name, "__gg__length") )
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("about to call \"")
      TRACE1_TEXT(function_name)
      TRACE1_TEXT("\"")
      TRACE1_INDENT
      TRACE1_REFER("parameter: ", ref1, "")
      }

    const charmap_t *charmap = __gg__get_charmap(ref1.field->codeset.encoding);
    tree stride = gg_cast(LONG, integer_one_node);

    switch(ref1.field->type)
      {
      case FldInvalid:
      case FldGroup:
      case FldAlphanumeric:
      case FldNumericDisplay:
      case FldNumericEdited:
      case FldAlphaEdited:
      case FldLiteralA:
        stride = build_int_cst_type(LONG, charmap->stride());
        break;

      case FldNumericBinary:
      case FldFloat:
      case FldPacked:
      case FldNumericBin5:
      case FldLiteralN:
      case FldClass:
      case FldConditional:
      case FldForward:
      case FldIndex:
      case FldSwitch:
      case FldDisplay:
      case FldPointer:
        stride = gg_cast(LONG, integer_one_node);
        break;
      }

    if( ref1.field->attr & hex_encoded_e )
      {
      stride = gg_cast(LONG, integer_one_node);
      }

    size_t upper = ref1.field->occurs.bounds.upper
                                    ? ref1.field->occurs.bounds.upper : 1;
    if( ref1.nsubscript() )
      {
      upper = 1;
      }

    if( is_table(ref1.field) && !ref1.nsubscript() )
      {
      static tree depending_on = gg_define_variable(LONG, "..pic1_dep");
      depending_on_value(depending_on, ref1.field);
      gg_call(VOID,
              "__gg__int128_to_field",
              gg_get_address_of(tgt->var_decl_node),
              gg_cast(INT128,
                      gg_divide(gg_multiply(refer_size_source(ref1),
                                            depending_on),
                                stride)),
              integer_zero_node,
              build_int_cst_type(INT, truncation_e),
              null_pointer_node,
              NULL_TREE );
      }
    else
      {
      if( upper == 1 )
        {
        gg_call(VOID,
                "__gg__int128_to_field",
                gg_get_address_of(tgt->var_decl_node),
                gg_cast(INT128,
                        gg_divide(refer_size_source(ref1),
                                  stride)),
                integer_zero_node,
                build_int_cst_type(INT, truncation_e),
                null_pointer_node,
                NULL_TREE );
        }
      else
        {
        gg_call(VOID,
                "__gg__int128_to_field",
                gg_get_address_of(tgt->var_decl_node),
                gg_cast(INT128,
                        gg_divide(gg_multiply(refer_size_source(ref1),
                                            build_int_cst_type(LONG, upper)),
                                  stride)),
                integer_zero_node,
                build_int_cst_type(INT, truncation_e),
                null_pointer_node,
                NULL_TREE );
        }
      }
    }
  else if( strcmp(function_name, "__gg__char") == 0 )
    {
    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            gg_get_address_of(ref1.field->var_decl_node),
            refer_offset(ref1),
            refer_size_source(ref1),
            NULL_TREE);
    }
  else
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("about to call \"")
      TRACE1_TEXT(function_name)
      TRACE1_TEXT("\"")
      TRACE1_INDENT
      TRACE1_REFER("parameter: ", ref1, "")
      }

    gg_call(VOID,
            function_name,
            gg_get_address_of(tgt->var_decl_node),
            gg_get_address_of(ref1.field->var_decl_node),
            refer_offset(ref1),
            refer_size_source(ref1),
            NULL_TREE);
    }

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

static bool
handle_gg_trim(cbl_field_t *tgt,
               const cbl_refer_t& input,
               size_t how,
               const std::vector<cbl_refer_t>& args )
  {
  bool handled = false;
  charmap_t *charmap = __gg__get_charmap(input.field->codeset.encoding);
    {
    if(charmap->stride() == 1 && !charmap->is_like_utf8() )
      {
      size_t array_size = args.size();
      tree charstype = build_array_type_nelts(UCHAR, array_size);
      tree chars     = gg_define_variable( charstype,
                                           NULL,
                                           vs_stack);
      TREE_ADDRESSABLE (chars) = 1;
      tree char_p    = gg_define_variable(UCHAR_P);
      gg_assign(char_p, gg_pointer_to_array (chars));

      for(const auto& arg : args)
        {
        cbl_figconst_t figconst = static_cast<cbl_figconst_t>
                                            (arg.field->attr & FIGCONST_MASK);
        if( figconst )
          {
          uint8_t figcst = charmap->figconst_character(figconst);
          tree tfigcst = build_int_cst_type(UCHAR, figcst);
          gg_assign(gg_indirect(char_p), tfigcst);
          }
        else
          {
          tree location;
          get_location(location, arg);
          gg_assign(gg_indirect(char_p), gg_indirect(location));
          }
        gg_increment(char_p);
        }

      gg_call(VOID,
              "__gg__trim_1",
              gg_get_address_of(tgt->var_decl_node),
              gg_get_address_of(input.field->var_decl_node),
              refer_offset(input),
              refer_size_source(input),
              gg_pointer_to_array(chars),
              build_int_cst_type(INT, (args.size()<<8) + how),
              NULL_TREE);
      handled = true;
      }
    }
  return handled;
  }

void
parser_trim( cbl_field_t *tgt,
             const cbl_refer_t& input,
             size_t how,
             const std::vector<cbl_refer_t>& args )
  {
  RETURN_IF_PARSE_ONLY;
  gcc_assert(how >= 1 && how <= 3);
  if( !handle_gg_trim(tgt, input, how, args) )
    {
    // We know stride is bigger than 1.
    cbl_encoding_t encoding = input.field->codeset.encoding;
    const charmap_t *charmap = __gg__get_charmap(encoding);
    int stride = charmap->stride();
    tree tstride = build_int_cst_type(SIZE_T, stride);

    size_t array_size = args.size() * stride;
    tree charstype = build_array_type_nelts(CHAR, array_size);
    tree chars     = gg_define_variable( charstype,
                                         NULL,
                                         vs_stack);
    TREE_ADDRESSABLE (chars) = 1;
    tree char_p    = gg_define_variable(CHAR_P);
    gg_assign(char_p, gg_pointer_to_array (chars));

    for(const auto& arg : args)
      {
      cbl_figconst_t figconst = static_cast<cbl_figconst_t>
                                          (arg.field->attr & FIGCONST_MASK);
      if( figconst )
        {
        char space[] = " ";
        *space = char_from_figconst(figconst);

        // Convert that character to the encoded version:
        size_t nbytes;
        const char *converted =  __gg__iconverter(DEFAULT_SOURCE_ENCODING,
                                                  encoding,
                                                  space,
                                                  1,
                                                  &nbytes);
        // And add it to the array:
        gg_memcpy(char_p,
                  build_string_literal(stride, converted),
                  tstride);
        }
      else
        {
        // It's not a figurative constant, so we get our value from 'arg'
        tree location;
        get_location(location, arg);
        gg_memcpy(char_p,
                  location,
                  tstride);
        }
      gg_assign(char_p, gg_add(char_p, tstride));
      }
    gg_call(VOID,
            "__gg__trim_a",
            gg_get_address_of(tgt->var_decl_node),
            gg_get_address_of(input.field->var_decl_node),
            refer_offset(input),
            refer_size_source(input),
            gg_pointer_to_array (chars),
            build_int_cst_type(SIZE_T, array_size),
            build_int_cst_type(INT, how),
            NULL_TREE);
    }
  }

void
parser_intrinsic_call_2( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    TRACE1_INDENT
    TRACE1_REFER("parameter 1: ", ref1, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 2: ", ref2, "")
    }
  store_location_stuff(function_name);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          gg_get_address_of(ref1.field->var_decl_node),
          refer_offset(ref1),
          refer_size_source(ref1),
          ref2.field ? gg_get_address_of(ref2.field->var_decl_node)
                     : null_pointer_node,
          refer_offset(ref2),
          refer_size_source(ref2),
          NULL_TREE);

  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_3( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2,
                       cbl_refer_t& ref3 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    TRACE1_INDENT
    TRACE1_REFER("parameter 1: ", ref1, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 2: ", ref2, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 3: ", ref3, "")
    }

  store_location_stuff(function_name);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          ref1.field ? gg_get_address_of(ref1.field->var_decl_node) : null_pointer_node,
          refer_offset(ref1),
          refer_size_source(ref1),
          ref2.field ? gg_get_address_of(ref2.field->var_decl_node) : null_pointer_node,
          refer_offset(ref2),
          refer_size_source(ref2),
          ref3.field ? gg_get_address_of(ref3.field->var_decl_node) : null_pointer_node,
          refer_offset(ref3),
          refer_size_source(ref3),
          NULL_TREE);
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

void
parser_intrinsic_call_4( cbl_field_t *tgt,
                       const char function_name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2,
                       cbl_refer_t& ref3,
                       cbl_refer_t& ref4 )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" of ")
    SHOW_PARSE_TEXT(function_name)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT("about to call \"")
    TRACE1_TEXT(function_name)
    TRACE1_TEXT("\"")
    TRACE1_INDENT
    TRACE1_REFER("parameter 1: ", ref1, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 2: ", ref2, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 3: ", ref3, "")
    TRACE1_INDENT
    TRACE1_REFER("parameter 4: ", ref4, "")
    }
  store_location_stuff(function_name);

  gg_call(VOID,
          function_name,
          gg_get_address_of(tgt->var_decl_node),
          ref1.field ? gg_get_address_of(ref1.field->var_decl_node) : null_pointer_node,
          refer_offset(ref1),
          refer_size_source(ref1),
          ref2.field ? gg_get_address_of(ref2.field->var_decl_node) : null_pointer_node,
          refer_offset(ref2),
          refer_size_source(ref2),
          ref3.field ? gg_get_address_of(ref3.field->var_decl_node) : null_pointer_node,
          refer_offset(ref3),
          refer_size_source(ref3),
          ref4.field ? gg_get_address_of(ref4.field->var_decl_node) : null_pointer_node,
          refer_offset(ref4),
          refer_size_source(ref4),
          NULL_TREE);
  TRACE1
    {
    TRACE1_INDENT
    TRACE1_FIELD("result: ", tgt, "")
    TRACE1_END
    }
  }

static void
field_increment(cbl_field_t *fld )
  {
  // rdigits has to be zero.
  tree value;
  get_binary_value(value, fld, INT128);
  gg_increment(value);
  gg_call(VOID,
          "__gg__int128_to_field",
          gg_get_address_of(fld->var_decl_node),
          gg_cast(INT128, value),
          integer_zero_node,
          build_int_cst_type(INT, truncation_e),
          null_pointer_node,
          NULL_TREE );
  }

static void
create_lsearch_address_pairs(struct cbl_label_t *name)
  {
  // Create the lsearch structure
  name->structs.lsearch =
                  static_cast<cbl_lsearch_t *>(xmalloc(sizeof(cbl_lsearch_t)));
  gcc_assert(name->structs.lsearch);
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  gg_create_goto_pair(&lsearch->addresses.at_exit.go_to,
                      &lsearch->addresses.at_exit.label);

  gg_create_goto_pair(&lsearch->addresses.top.go_to,
                      &lsearch->addresses.top.label);

  gg_create_goto_pair(&lsearch->addresses.bottom.go_to,
                      &lsearch->addresses.bottom.label);
  }

void
parser_next_sentence()
  {
  // Eventually we'll need this.
  }

void
parser_lsearch_start(   cbl_label_t *name,
                        cbl_field_t *table,
                        cbl_field_t *index,
                        cbl_field_t *varying )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    if( table )
      {
      SHOW_PARSE_TEXT(" linear search of ")
      SHOW_PARSE_TEXT(table->name)
      }
    if( index )
      {
      SHOW_PARSE_TEXT(" index is ")
      SHOW_PARSE_TEXT(index->name)
      }
    if( varying )
      {
      SHOW_PARSE_TEXT(" varying ")
      SHOW_PARSE_TEXT(varying->name)
      }
    SHOW_PARSE_END
    }
  // Create the goto/label pairs we are going to be needing:
  create_lsearch_address_pairs(name);
  cbl_lsearch_t *lsearch = name->structs.lsearch;
  lsearch->first_when = true;

  // We need to find the first table element:
  cbl_field_t *current = table;
  while(current)
    {
    if( is_table(current) )
      {
      // Extract the number of elements in that rightmost dimension.
      lsearch->limit = gg_define_variable(LONG);
      depending_on_value(lsearch->limit, current);
      break;
      }
    current = parent_of(current);
    }

  // Establish the initial value of our counter:
  lsearch->counter = gg_define_variable(LONG);

  tree value;
  if(varying)
    {
    get_binary_value(value, varying, SIZE_T);
    }
  else if( index )
    {
    get_binary_value(value, index, SIZE_T);
    }
  else
    {
    gcc_unreachable();
    }
  gg_assign(lsearch->counter, gg_cast(LONG, value));

  // And we need these around, so we can increment them:
  lsearch->index = index;
  lsearch->varying = varying;

  // From here we have to jump to the top of the loop:
  gg_append_statement(lsearch->addresses.top.go_to);

  // The next next instructions will be the body of the at-exit code, so
  // we need a label here so that we can get back to them
  gg_append_statement(lsearch->addresses.at_exit.label);
  }

void
parser_lsearch_conditional(cbl_label_t * name)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  if( lsearch->first_when )
    {
    lsearch->first_when = false;
    // We are the first of the WHEN CONDITIONALs, which means we just laid down the final
    // statement of the AT-EXIT imperative statements, which means it's
    // time to leave the SEARCH completely.
    gg_append_statement(lsearch->addresses.bottom.go_to);

    // And that puts us at the top of the loop:
    gg_append_statement(lsearch->addresses.top.label);

    // It is at this point we check to see if we have reached the limit:
    IF( lsearch->counter, gt_op, lsearch->limit )
    // The counter has run out.
    gg_append_statement(lsearch->addresses.at_exit.go_to);
    ELSE
    // Just fall through into the following statements, which are
    // the statements for the conditional for the first WHEN
    ENDIF
    }
  else
    {
    // We are at the end of a WHEN TRUE imperative statement.
    gg_append_statement(lsearch->addresses.bottom.go_to);

    // This is the second or later search_conditional.  Note that the
    // code generated here executes after the first parser_when call, so
    // the jump_over label is ready to be placed.

    // We have to lay down the unnamed label so the prior WHEN can jump past
    // its imperative statements when its condition is not met:
    gg_append_statement(lsearch->jump_over.label);
    }
  // At this point, the parser starts laying down the statements that make
  // up the next conditional.
  }

void
parser_lsearch_when( cbl_label_t *name, cbl_field_t *conditional )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  // Arriving here means that all of the conditional statements have been
  // laid down, and we are ready to do the WHEN test:

  parser_if(conditional);
  // We have found what we were looking for.  Fall through to the next
  // set of instructions, which comprise the imperative statement
  // associated with the WHEN condition.
  ELSE
  // The conditional is false. We thus want to skip over the imperative
  // instructions that are about to be laid down.

  // Create an unnamed goto/label pair:
  gg_create_goto_pair(&lsearch->jump_over.go_to,
                      &lsearch->jump_over.label);

  // And lay down the goto.
  gg_append_statement(lsearch->jump_over.go_to);
  ENDIF
  }

void
parser_lsearch_end( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_lsearch_t *lsearch = name->structs.lsearch;

  // Arriving here means we have just laid down the final imperative
  // statements of the final WHEN.  If these statements have been executing,
  // it's now time to leave the SEARCH:
  gg_append_statement(lsearch->addresses.bottom.go_to);

  // It's time to lay down the last jump_over label:
  gg_append_statement(lsearch->jump_over.label);

  // With that in place, we increment stuff:
  gg_assign(lsearch->counter, gg_add(lsearch->counter, gg_cast(LONG, integer_one_node)));
  field_increment(lsearch->index);

  if( lsearch->varying )
    {
    field_increment(lsearch->varying);
    }
  // From here we jump to the top of the loop:
  gg_append_statement(lsearch->addresses.top.go_to);

  // And that means we now lay down the label for the bottom
  gg_append_statement(lsearch->addresses.bottom.label);

  // At this point, we are done with the lsearch structure
  free(lsearch);
  lsearch = NULL;
  }

void
parser_bsearch_start(   cbl_label_t* name,
                        cbl_field_t *table )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    if( table )
      {
      SHOW_PARSE_TEXT(" binary search of ")
      SHOW_PARSE_TEXT(table->name)
      }
    SHOW_PARSE_END
    }

  // We need a cbl_bsearch_t structure:
  name->structs.bsearch =
                  static_cast<cbl_bsearch_t *>(xmalloc(sizeof(cbl_bsearch_t)));
  gcc_assert(name->structs.bsearch);
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  // Create the address/label pairs we need
  gg_create_goto_pair(&bsearch->too_small.go_to,
                      &bsearch->too_small.label);

  gg_create_goto_pair(&bsearch->too_big.go_to,
                      &bsearch->too_big.label);

  gg_create_goto_pair(&bsearch->top.go_to,
                      &bsearch->top.label);

  gg_create_goto_pair(&bsearch->first_test.go_to,
                      &bsearch->first_test.label);

  gg_create_goto_pair(&bsearch->bottom.go_to,
                      &bsearch->bottom.label);

  // The logic when we first hit a WHEN needs to be different:
  bsearch->first_when = true;

  // We need to find our table element:
  cbl_field_t *current = table;
  while(current)
    {
    if( is_table(current) )
      {
      break;
      }
    current = parent_of(current);
    }

  CHECK_FIELD(current);

  // There are a number of things we learn from the field "current"

  // We get the index:
  gcc_assert(current->occurs.indexes.nfield);
  size_t index_index = current->occurs.indexes.fields[0];
  bsearch->index = cbl_field_of( symbol_at(index_index) );
  gcc_assert(bsearch->index);

  // And we get the rightward bound of the number of elements:
  // Not that these are LONGS, not SIZE_T.  If we are searching for something
  // that is smaller than element[0] of the table, then right ends up being
  // -1, so we have to have a signed type.
  bsearch->left   = gg_define_variable(LONG, "_left");
  bsearch->right  = gg_define_variable(LONG, "_right");
  bsearch->middle = gg_define_variable(LONG, "_middle");

  // Assign the left and right values:
  gg_assign(bsearch->left, build_int_cst_type(LONG, 1));
  depending_on_value(bsearch->right, current);

  // We now jump to the top of the binary testing loop, which comes right
  // after the labels where we handle non-equal cases:
  gg_append_statement(bsearch->top.go_to);

  gg_append_statement(bsearch->too_small.label);
  // Arrive here when the element in the array is smaller than the one we are
  // looking for.  This means that we move bsearch->left to the right:
  gg_assign(bsearch->left, gg_add(bsearch->middle, build_int_cst_type(LONG, 1)));
  gg_append_statement(bsearch->top.go_to);

  gg_append_statement(bsearch->too_big.label);
  // Arrive here when the element in the array is larger than the one we
  // are looking for.  This means we have to move bsearch->right to the left:
  gg_assign(bsearch->right, gg_subtract(bsearch->middle, build_int_cst_type(LONG, 1)));
  // Fall through to TOP:

  gg_append_statement(bsearch->top.label);
  // Arrive here when it is time to check to see if we are done:
  IF( bsearch->left, le_op, bsearch->right )
  // We are not done.  Calculate middle from 'left' and 'right'
  gg_assign(  bsearch->middle,
              gg_add(bsearch->left, bsearch->right) );
  gg_assign(  bsearch->middle,
              gg_divide(bsearch->middle, build_int_cst_type(LONG, 2) ));
  //gg_printf("BSEARCH At the top %ld %ld %ld\n", bsearch->left, bsearch->middle, bsearch->right, NULL_TREE);
  // We need to assign that value to bsearch->index.  It might be possible
  // to assume that bsearch->index is a size_t and just cram the bytes into
  // place at bsearch->index->var_decl_node->data.  But for now we'll
  // be cautious and use the slower, but more assured, method:

  gg_call(VOID,
          "__gg__int128_to_field",
          gg_get_address_of(bsearch->index->var_decl_node),
          gg_cast(INT128, bsearch->middle),
          integer_zero_node,
          build_int_cst_type(INT, truncation_e),
          null_pointer_node,
          NULL_TREE );
  // And with middle/index established, we go do the WHEN clause:
  gg_append_statement(bsearch->first_test.go_to);
  ELSE
  // The search ended without finding anything.  Fall through to the
  // AT-EXIT imperative statements that the parser will lay down right
  // after the call to parser_bsearch_start().
  ENDIF
  }

void
parser_bsearch_conditional( cbl_label_t* name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  if( bsearch->first_when )
    {
    bsearch->first_when = false;
    // The first time we arrive here is after the WHEN part of the SEARCH ALL
    // statement.  We have just finished executing any AT-END statements there
    // might be, so it's time to jump to the bottom:
    gg_append_statement(bsearch->bottom.go_to);

    // Otherwise, the TOP part of the loop just calculated the next middle/index,
    // and we now start processing it

    gg_append_statement(bsearch->first_test.label);
    }
  // The second parser_bsearch_conditional() is caused by the appearance of
  // any subsequent AND clauses.  And, it turns out, we do nothing.

  // The parser lays down the statements that calculate the conditional,
  // and we just wait for parser_bsearch_when()
  }

bool
is_ascending_key(const cbl_refer_t& key)
  {
  bool retval = true;

  cbl_field_t *family_tree = key.field;
  while( family_tree )
    {
    if( family_tree->occurs.nkey )
      {
      break;
      }
    family_tree = parent_of(family_tree);
    }

  CHECK_FIELD(family_tree);
  gcc_assert(family_tree->occurs.nkey);

  for(size_t i=0; i<family_tree->occurs.nkey; i++)
    {
    for(size_t j=0; j<family_tree->occurs.keys[i].field_list.nfield; j++)
      {
      size_t index_of_field
        = family_tree->occurs.keys[i].field_list.fields[j];
      const cbl_field_t *key_field = cbl_field_of(symbol_at(index_of_field));

      if( strcmp( key_field->name,
                  key.field->name ) == 0 )
        {
        retval = family_tree->occurs.keys[i].ascending;
        goto done;
        }
      }
    }

done:
  return retval;
  }

void
parser_bsearch_when(cbl_label_t* name,
              const cbl_refer_t &key,
              const cbl_refer_t &sarg,
                    bool ascending)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  tree left;
  tree right;
  if( ascending )
    {
    cobol_compare(left, right, key, sarg);
    }
  else
    {
    cobol_compare(left, right, sarg, key);
    }

  IF( left, lt_op, right )
    {
    gg_append_statement(bsearch->too_small.go_to);
    }
  ELSE
    {
    IF( left, gt_op, right )
      {
      gg_append_statement(bsearch->too_big.go_to);
      }
    ELSE
      {
      }
    ENDIF
    }
  ENDIF

  // We are at the Goldilocks point.  The clause has been satisfied with
  // an equality, so we will just fall through to the next set of statements
  // that the parser laid down.  They are either the next conditional, or
  // the final imperative statements that get executed when all the
  // clauses are satisfied.
  }

void
parser_bsearch_end( cbl_label_t* name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( name )
      {
      SHOW_PARSE_TEXT(" ")
      SHOW_PARSE_TEXT(name->name)
      }
    SHOW_PARSE_END
    }
  cbl_bsearch_t *bsearch = name->structs.bsearch;

  // Arriving here means that either the search ran out without finding
  // anything, (see the test up at TOP:), or else we just fell through from
  // the statements that executed after all the WHEN/AFTER clauses were
  // satisfied by equality (meaning there were no jumps to TOO_SMALL: or
  // TOO_LARGE).  In other words: we're done.
  gg_append_statement(bsearch->bottom.label);

  free(bsearch);
  }

tree
gg_array_of_field_pointers( const std::vector<const cbl_field_t *> &fields )
  {
  size_t N = fields.size();
  gcc_assert(N);

  tree const_field_pointer_type =
    build_qualified_type( cblc_field_p_type_node,
                          TYPE_QUAL_CONST );

  tree array_type =
    build_array_type_nelts( const_field_pointer_type, N );

  vec<constructor_elt, va_gc> *elts = NULL;
  vec_alloc( elts, N );

  for( size_t i=0; i<N; i++ )
    {
    tree field_pointer = fields[i] && fields[i]->var_decl_node
                       ? gg_get_address_of( fields[i]->var_decl_node)
                       : null_pointer_node;

    field_pointer = gg_cast( cblc_field_p_type_node, field_pointer );

    CONSTRUCTOR_APPEND_ELT( elts,
                            bitsize_int( i ),
                            field_pointer );
    }

  tree constr = build_constructor( array_type, elts );

  tree retval = gg_define_variable( array_type );

  TREE_READONLY( retval ) = 1;
  DECL_INITIAL( retval ) = constr;

  return gg_pointer_to_array(retval);
  }

tree
gg_array_of_uchar_p( const std::vector<tree> &uchar_p )
  {
  size_t N = uchar_p.size();
  if( !N )
    {
    return null_pointer_node;
    }

  tree const_uchar_p_type =
    build_qualified_type( UCHAR_P,
                          TYPE_QUAL_CONST );

  tree array_type =
    build_array_type_nelts( const_uchar_p_type, N );

  vec<constructor_elt, va_gc> *elts = NULL;
  vec_alloc( elts, N );

  for( size_t i=0; i<N; i++ )
    {
    CONSTRUCTOR_APPEND_ELT( elts,
                            bitsize_int( i ),
                            uchar_p[i] );
    }

  tree constr = build_constructor( array_type, elts );
  tree retval = gg_define_variable( array_type );
  TREE_READONLY( retval ) = 1;
  DECL_INITIAL( retval ) = constr;

  // Return a pointer to the first element:
  return gg_pointer_to_array(retval);
  }


static void
push_program_state()
  {
  gg_call(VOID,
          "__gg__push_program_state",
          NULL_TREE);
  }

static void
pop_program_state()
  {
  gg_call(VOID,
          "__gg__pop_program_state",
          NULL_TREE);
  }

void
parser_sort(cbl_refer_t tableref,
            bool duplicates,
            cbl_alphabet_t *alphabet,
            const std::vector<cbl_key_t>& keys )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( tableref.field )
      {
      SHOW_PARSE_REF(" Sort table: ", tableref)
      }
    SHOW_PARSE_END
    }

  cbl_field_t *table = tableref.field;
  gcc_assert(table);
  gcc_assert(table->var_decl_node);
  if( !is_table(table) )
    {
    cbl_internal_error(  "%s: asked to sort %s, which is not a table",
            __func__,
            tableref.field->name);
    }

  std::vector<const cbl_field_t *>flattened_fields_2;
  std::vector<size_t>flattened_ascending_2;
  for( size_t i=0; i<keys.size(); i++ )
    {
    for( size_t j=0; j<keys[i].fields.size(); j++ )
      {
      flattened_fields_2.push_back(keys[i].fields[j]);
      flattened_ascending_2.push_back(keys[i].ascending ? 1 : 0);
      }
    }

  tree all_keys = gg_array_of_field_pointers(flattened_fields_2);

  // Create the array of integers that are the flags for ASCENDING:
  tree ascending = gg_array_of_size_t(flattened_ascending_2 );

  tree depending_on = gg_define_variable(LONG, "_sort_size");
  depending_on_value(depending_on, table);

  if( alphabet )
    {
    push_program_state();
    parser_alphabet_use(alphabet);
    }
  gg_call(VOID,
          "__gg__sort_table",
          gg_get_address_of(tableref.field->var_decl_node),
          refer_offset(tableref),
          gg_cast(SIZE_T, depending_on),
          build_int_cst_type(SIZE_T, flattened_fields_2.size()),
          all_keys,
          ascending,
          duplicates ? integer_one_node : integer_zero_node,
          NULL_TREE);
  if( alphabet )
    {
    pop_program_state();
    }
  }

void
parser_file_sort(   cbl_file_t *workfile,
                    bool duplicates,
                    cbl_alphabet_t *alphabet,
                    const std::vector<cbl_key_t>& keys,
                    size_t ninput,
                    cbl_file_t **inputs,
                    size_t noutput,
                    cbl_file_t **outputs,
                    cbl_perform_tgt_t *in_proc,
                    cbl_perform_tgt_t *out_proc )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // This is the implementation of SORT FORMAT 1

  // It proceeds in three phases.

  // The first phase is absorbing the input and writing it out to the workfile:

  parser_file_open(workfile, 'w');
  IF( member(workfile, "io_status"), ge_op, build_int_cst_type(INT, FsEofSeq) )
    {
    gg_printf("Couldn't open the SORT workfile for writing\n", NULL_TREE);
    gg_exit(integer_one_node);
    }
  ELSE
    ENDIF

  if( in_proc && !ninput )
    {
    // We are getting our inputs from an input procedure
    parser_perform(in_proc, NULL);
    }
  else if( ninput && !in_proc )
    {
    // ninput means there was a USING clause, specifying input files.

    // We are going to transfer the input file[s] to the workfile.  The
    // transfer will be done so that any newlines in a LINE SEQUENTIAL file
    // are skipped, and so that any records that are too long, or too short,
    // are all normalized to the format of the SD record.
    for(size_t i=0; i<ninput; i++)
      {
      parser_file_open(inputs[i], 'r');
      IF( member(workfile, "io_status"), ge_op, build_int_cst_type(INT, FsEofSeq) )
        {
        gg_printf("Couldn't open the SORT USING file for input\n", NULL_TREE);
        gg_exit(integer_one_node);
        }
      ELSE
        ENDIF

      gg_call(VOID,
              "__gg__file_sort_ff_input",
              gg_get_address_of(workfile-> var_decl_node),
              gg_get_address_of(inputs[i]->var_decl_node),
              NULL_TREE);
      parser_file_close(inputs[i]);
      }
    }
  else
    {
    // Having both or neither violates SORT syntax
    cbl_internal_error("%s: syntax error: both (or neither) USING "
          "and input-proc are specified",
          __func__);
    }
  parser_file_close(workfile);

  // At this point, we have workfile of unsorted data.  We have a library
  // routine that sorts the workfile.  It needs the keys:

  // The following is a tad more complex than it needs to be.  It's a partial
  // clone of the code for handling multiple keys, each of which can have
  // multiple fields.

  std::vector<const cbl_field_t *>flattened_fields_2;
  std::vector<size_t>flattened_ascending_2;
  for( size_t i=0; i<keys.size(); i++ )
    {
    for( size_t j=0; j<keys[i].fields.size(); j++ )
      {
      flattened_fields_2.push_back(keys[i].fields[j]);
      flattened_ascending_2.push_back(keys[i].ascending ? 1 : 0);
      }
    }

  // Create the array of cbl_field_t pointers for the keys
  tree all_keys = gg_array_of_field_pointers(flattened_fields_2);

  // Create the array of integers that are the flags for ASCENDING:
  tree ascending = gg_array_of_size_t(flattened_ascending_2 );

  // We need to open the workfile for the sorting routine:
  parser_file_open(workfile, 'r');
  IF( member(workfile, "io_status"),
      ge_op,
      build_int_cst(INT, FhNotOkay) )
    {
    rt_error("Couldn't open workfile for sorting in parser_file_sort\n");
    }
  ELSE
  ENDIF
  if( alphabet )
    {
    push_program_state();
    parser_alphabet_use(alphabet);
    }
  gg_call(VOID,
          "__gg__sort_workfile",
          gg_get_address_of(workfile->var_decl_node),
          build_int_cst_type(SIZE_T, flattened_fields_2.size()),
          all_keys,
          ascending,
          duplicates ? integer_one_node : integer_zero_node,
          NULL_TREE);
  if( alphabet )
    {
    pop_program_state();
    }
  parser_file_close(workfile);

  // The workfile is sorted.  We move to Phase 3 -- transferring the workfile
  // to the output.

  if( noutput && !out_proc)
    {
    // We have a GIVING phrase:
    for(size_t i=0; i<noutput; i++)
      {
      // Open WORKFILE again to position it at the beginning
      parser_file_open(workfile, 'r');
      IF( member(workfile, "io_status"),
          ge_op,
          build_int_cst(INT, FhNotOkay) )
        {
        rt_error("Couldn't open workfile for transfer to GIVING"
                  "in parser_file_sort");
        }
      ELSE
      ENDIF
      parser_file_open(outputs[i], 'w');
      IF( member(outputs[i], "io_status"),
          ge_op,
          build_int_cst(INT, FhNotOkay) )
        {
        rt_error("Couldn't open GIVING file in parser_file_sort");
        }
      ELSE
      ENDIF
      gg_call(VOID,
              "__gg__file_sort_ff_output",
              gg_get_address_of(outputs[i]->var_decl_node),
              gg_get_address_of(workfile->var_decl_node),
              NULL_TREE);
      parser_file_close(outputs[i]);
      parser_file_close(workfile);
      }
    }
  else if (!noutput && out_proc)
    {
    // We are going to transfer the workfile to the output procedures.
    parser_file_open(workfile,'r');
    IF( member(workfile, "io_status"),
        ge_op,
        build_int_cst(INT, FhNotOkay) )
      {
      rt_error("Couldn't open workfile for stage-three "
                "output in parser_file_sort");
      }
    ELSE
      {
      parser_perform(out_proc, NULL);
      parser_file_close(workfile);
      }
    ENDIF
    }
  else
    {
    cbl_internal_error("%s: syntax error: both (or neither) GIVING "
          "and output-proc are specified", __func__);
    }
  }

void
parser_release( const cbl_field_t *record_area )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // When this routine is called, it writes the contents of 'record_area' to the
  // workfile specified by the cbl_file_t parent of record_area:

  cbl_file_t *workfile = symbol_record_file(record_area);

  gg_call(VOID,
          "__gg__file_write",
          gg_get_address_of( workfile->var_decl_node),
          member(record_area, "data"),
          member(record_area, "capacity"),
          integer_zero_node,
          integer_minusone_node,
          integer_zero_node,
          NULL_TREE); // non-random
  set_user_status(workfile);
  }

void
parser_return_start( cbl_file_t *workfile, cbl_refer_t into )
  {
  Analyze();
  // This function helps implement the COBOL RETURN statement, which is used
  // in SORT and MERGE to "return" data from an intermediate sort/merge file
  // to SORT/MERGE output procedure.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // We assume that workfile is open.

  workfile->addresses = static_cast<cbl_sortreturn_t *>
                                          (xmalloc(sizeof(cbl_sortreturn_t)));
  gcc_assert(workfile->addresses);
  gg_create_goto_pair(&workfile->addresses->at_end.go_to,
                      &workfile->addresses->at_end.label);
  gg_create_goto_pair(&workfile->addresses->not_at_end.go_to,
                      &workfile->addresses->not_at_end.label);
  gg_create_goto_pair(&workfile->addresses->bottom.go_to,
                      &workfile->addresses->bottom.label);

  // Read the data from workfile into the SD record position:
  cbl_field_t *data_location = symbol_file_record(workfile);
  parser_file_read(workfile, data_location, -1 );

  // And jump to either at_end or not_at_end, depending:
  IF( member(workfile, "io_status"), lt_op, build_int_cst(INT, FsEofSeq) )
    {
    // The read was successful.  We move the result into place
    if( into.field )
      {
      cbl_field_t *record_area =
                             cbl_field_of(symbol_at(workfile->default_record));
      parser_move(into, record_area, truncation_e);
      }
    // And having moved -- or not -- the record, jump to the not-at-end
    // imperative
    gg_append_statement(workfile->addresses->not_at_end.go_to);
    }
  ELSE
    ENDIF

  IF( member(workfile, "io_status"), lt_op, build_int_cst(INT, FsKeySeq) )
    {
    // The read didn't succeed because of an end-of-file condition.

    // Because there is an AT END clause, we suppress the error condition that
    // was raised.
    gg_assign(var_decl_exception_code, integer_zero_node);

    // And then we jump to the at_end code:
    gg_append_statement(workfile->addresses->at_end.go_to);
    }
  ELSE
    ENDIF

  // Arriving here means some kind of error condition.  So, we don't do the
  // move, and we jump to the end of the statement
  gg_append_statement(workfile->addresses->bottom.go_to);
  }

void
parser_return_atend( cbl_file_t *workfile )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // There might or might not be an at_end clause, and it might, or might
  // not, appear after a not_at_end clause.  If we are appearing after
  // a not_at_end clause, we need to finish that clause with a jump to the
  // bottom of the logic:
  if( !workfile->addresses->not_at_end.label )
    {
    // We have been preceded by a not_at_end label.  So, we need to
    // put in a jump to end those statements:
    gg_append_statement(workfile->addresses->bottom.go_to);
    }
  // And now we place the at_end label:
  gg_append_statement(workfile->addresses->at_end.label);

  // And having placed it, NULL it out
  workfile->addresses->at_end.label = NULL;

  // The imperative statements of the NOT AT END clause will follow
  }

void
parser_return_notatend( cbl_file_t *workfile )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  // There might or might not be a not_at_end clause, and it might, or might
  // not, appear after a at_end clause.  If we are appearing after
  // a at_end clause, we need to finish that clause with a jump to the
  // bottom of the logic:
  if( !workfile->addresses->at_end.label )
    {
    // We have been preceded by an at_end label.  So, we need to
    // put in a jump to end those statements:
    gg_append_statement(workfile->addresses->bottom.go_to);
    }
  // And now we place the not_at_end label:
  gg_append_statement(workfile->addresses->not_at_end.label);

  // And having placed it, NULL it out
  workfile->addresses->not_at_end.label = NULL;

  // The imperative statements of the AT END clause will follow
  }

void
parser_return_finish( cbl_file_t *workfile )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // If we are preceded by either an at_end or not_at_end clause, we need
  // to end those statements with a jump to the bottom:
  if( !workfile->addresses->at_end.label || !workfile->addresses->not_at_end.label)
    {
    gg_append_statement(workfile->addresses->bottom.go_to);
    }

  // We need to place labels for clauses that weren't explicitly expressed
  // in the COBOL source code.  (Both were explicit targets of goto statements
  // back in parser_return_start, so we need to place them here if they
  // weren't placed elsewhere)
  if( workfile->addresses->at_end.label )
    {
    gg_append_statement(workfile->addresses->at_end.label);
    }
  if( workfile->addresses->not_at_end.label )
    {
    gg_append_statement(workfile->addresses->not_at_end.label);
    }
  // And that brings us to the bottom:
  gg_append_statement(workfile->addresses->bottom.label);

  free(workfile->addresses);
  }

static tree
gg_array_of_file_pointers(  size_t N,
                            cbl_file_t **files )
  {
  tree retval = gg_define_variable(cblc_file_pp_type_node);
  gg_assign(retval, gg_cast(  cblc_file_pp_type_node,
                              gg_malloc(  build_int_cst_type(SIZE_T,
                                                             N * int_size_in_bytes(VOID_P)))));
  for(size_t i=0; i<N; i++)
    {
    gg_assign(gg_array_value(retval, i), gg_get_address_of(files[i]->var_decl_node));
    }
  return retval;
  }

void
parser_file_merge(  cbl_file_t *workfile,
              const cbl_alphabet_t *alphabet,
                    const std::vector<cbl_key_t>& keys,
                    size_t ninputs,
                    cbl_file_t **inputs,
                    size_t noutputs,
                    cbl_file_t **outputs,
                    cbl_perform_tgt_t *out_proc )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }

  // Our default file organization is LINE SEQUENTIAL, which spectacularly does
  // *not* work for a SORT workfile.
  if( workfile->org == file_line_sequential_e )
    {
    workfile->org = file_sequential_e;
    gg_assign(  member(workfile->var_decl_node, "org"),
                build_int_cst_type(INT, file_sequential_e));
    }

  std::vector<const cbl_field_t *>flattened_fields_2;
  std::vector<size_t>flattened_ascending_2;
  for( size_t i=0; i<keys.size(); i++ )
    {
    for( size_t j=0; j<keys[i].fields.size(); j++ )
      {
      flattened_fields_2.push_back(keys[i].fields[j]);
      flattened_ascending_2.push_back(keys[i].ascending ? 1 : 0);
      }
    }

  // Create the array of cbl_field_t pointers for the keys
  tree all_keys =  gg_array_of_field_pointers(flattened_fields_2);

  // Create the array of integers that are the flags for ASCENDING:
  tree ascending = gg_array_of_size_t(flattened_ascending_2 );

  tree all_files = gg_array_of_file_pointers(ninputs, inputs);

  // We need to open all of the input files and the workfile.  It's easiest to
  // do that here, rather than in the libgcobol, because of the possibility that
  // the filename is in a variable or an environment variable, rather than a
  // literal. This is handled by parser_file_open() in a way that would be
  // inconvenient in __gg__file_open

  parser_file_open(workfile, 'w');
  IF( member(workfile, "io_status"),
      ge_op,
      build_int_cst_type(INT, FhNotOkay) )
    {
    rt_error("Couldn't open workfile for stage-one "
              "writing in parser_file_merge");
    }
  ELSE
    ENDIF

  const cbl_enabled_exceptions_t&
                                enabled_exceptions( cdf_enabled_exceptions() );

  for(size_t i=0; i<ninputs; i++)
    {
    if( process_this_exception(ec_sort_merge_file_open_e) )
      {
      IF( member(inputs[i], "file_pointer"), ne_op, null_pointer_node )
        {
        if( enabled_exceptions.match(ec_sort_merge_file_open_e) )
          {
          set_exception_code(ec_sort_merge_file_open_e);
          }
        else
          {
          rt_error("FILE MERGE file not open");
          }
        }
      ELSE
        ENDIF
      }

    parser_file_open(inputs[i], 'r');
    IF( member(inputs[i], "io_status"),
        ge_op,
        build_int_cst_type(INT, FhNotOkay) )
      {
      char ach[128];
      sprintf(ach,
              "Couldn't open %s for stage-one reading in parser_file_merge",
              inputs[i]->name);
      rt_error(ach);
      }
    ELSE
      ENDIF
    }

  cbl_field_t *sd_record = symbol_file_record(workfile);
  if( alphabet )
    {
    push_program_state();
    parser_alphabet_use(alphabet);
    }
  gg_call(VOID,
          "__gg__merge_files",
          gg_get_address_of(workfile->var_decl_node),
          build_int_cst_type(SIZE_T, keys.size()),
          all_keys,
          ascending,
          build_int_cst_type(SIZE_T, ninputs),
          all_files,
          NULL_TREE);
  if( alphabet )
    {
    pop_program_state();
    }

  parser_file_close(workfile);
  for(size_t i=0; i<ninputs; i++)
    {
    parser_file_close(inputs[i]);
    }

  // The merged workfile has been created.
  if( noutputs && !out_proc)
    {
    // We are going to transfer the workfile to the output files.
    for(size_t i=0; i<noutputs; i++)
      {
      if( process_this_exception(ec_sort_merge_file_open_e) )
        {
        IF( member(outputs[i], "file_pointer"), ne_op, null_pointer_node )
          {
          if( enabled_exceptions.match(ec_sort_merge_file_open_e) )
            {
            set_exception_code(ec_sort_merge_file_open_e);
            }
          else
            {
            rt_error("FILE MERGE file not open");
            }
          }
        ELSE
          ENDIF
        }
      // We keep reopening the workfile as a convenient way to make sure it is
      // positioned at the beginning.
      parser_file_open(workfile,'r');
      IF( member(workfile, "io_status"),
          ge_op,
          build_int_cst_type(INT, FhNotOkay) )
        {
        rt_error("Couldn't open workfile for stage-three "
                  "reading in parser_file_merge\n");
        }
      ELSE
        ENDIF

      parser_file_open(outputs[i], 'w');
      IF( member(outputs[i], "io_status"),
          ge_op,
          build_int_cst_type(INT, FhNotOkay) )
        {
        rt_error("Couldn't open an output file in parser_file_merge");
        }
      ELSE
        ENDIF
      gg_call(VOID,
              "__gg__file_sort_ff_output",
              gg_get_address_of(outputs[i]->var_decl_node),
              gg_get_address_of(workfile->  var_decl_node),
              gg_get_address_of(sd_record-> var_decl_node),
              NULL_TREE);
      parser_file_close(outputs[i]);
      parser_file_close(workfile);
      }
    }
  else if (!noutputs && out_proc)
    {
    // We are going to transfer the workfile to the output procedures.
    parser_file_open(workfile,'r');
    IF( member(workfile, "io_status"),
        ge_op,
        build_int_cst_type(INT, FhNotOkay) )
      {
      rt_error("Couldn't open workfile for"
                         " stage-three output in parser_file_merge");
      }
    ELSE
      ENDIF
    parser_perform(out_proc, NULL);
    parser_file_close(workfile);
    }
  else
    {
    cbl_internal_error("%s: syntax error: both (or neither) "
          "files and output-proc are specified", __func__);
    }
  }

void
parser_string_overflow( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  /*
   *  parser_string_overflow is called 0-2 times before the associated
   *  parser_string.
   */

  name->structs.unstring
    = static_cast<cbl_unstring_t *>(xmalloc(sizeof(struct cbl_unstring_t)));
  gcc_assert(name->structs.unstring);

  // Set up the address pairs for this clause
  gg_create_goto_pair(&name->structs.unstring->over.go_to,
                      &name->structs.unstring->over.label);
  gg_create_goto_pair(&name->structs.unstring->into.go_to,
                      &name->structs.unstring->into.label);
  gg_create_goto_pair(&name->structs.unstring->bottom.go_to,
                      &name->structs.unstring->bottom.label);

  // Jump over the [NOT] ON OVERFLOW code that is about to be laid down
  gg_append_statement( name->structs.unstring->over.go_to );

  // Create the label that allows the following code to be executed at
  // the appropriate time.
  gg_append_statement( name->structs.unstring->into.label );
  }

void
parser_string_overflow_end( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  gg_append_statement( name->structs.unstring->bottom.go_to );
  }

void
parser_unstring(cbl_refer_t src,
                size_t ndelimited,
                cbl_refer_t *delimiteds,
                size_t noutputs,
                cbl_refer_t *outputs,
                cbl_refer_t *delimiters,
                cbl_refer_t *counts,
                cbl_refer_t pointer,
                cbl_refer_t tally,
                cbl_label_t *overflow,
                cbl_label_t *not_overflow )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  if( overflow )
    {
    gg_append_statement(overflow->structs.unstring->over.label);
    }
  if( not_overflow )
    {
    gg_append_statement(not_overflow->structs.unstring->over.label);
    }

  std::vector<cbl_refer_t> delims(ndelimited);
  char *alls = static_cast<char *>(xmalloc(ndelimited+1));
  gcc_assert(alls);
  for(size_t i=0; i<ndelimited; i++)
    {
    delims[i] = delimiteds[i];
    alls[i] = delimiteds[i].all ? '1' : '0' ;
    }
  alls[ndelimited] = '\0';

  tree t_alls         = build_string_literal(ndelimited+1, alls);

  tree ref_data       = build_array_of_referlets(ndelimited, delims.data());
  tree ref_outputs    = build_array_of_referlets(noutputs,   outputs);
  tree ref_delimiters = build_array_of_referlets(noutputs,   delimiters);
  tree ref_counts     = build_array_of_referlets(noutputs,   counts);

  tree t_overflow = gg_define_variable(INT);
  gg_assign(t_overflow,
            gg_call_expr( INT,
                          "__gg__unstring",
                          ref_data,
                          ref_outputs,
                          ref_delimiters,
                          ref_counts,
                          gg_get_address_of(src.field->var_decl_node),
                          refer_offset(src),
                          refer_size_source(src),
                          build_int_cst_type(SIZE_T, ndelimited),
                          t_alls,
                          build_int_cst_type(SIZE_T, noutputs),
                          pointer.field ? gg_get_address_of(pointer.field->var_decl_node) : null_pointer_node,
                          refer_offset(pointer),
                          refer_size_dest(pointer),
                          tally.field ? gg_get_address_of(tally.field->var_decl_node) : null_pointer_node,
                          refer_offset(tally),
                          refer_size_dest(tally),
                          NULL_TREE)
                          );
  free(alls);

  if( overflow )
    {
    // We have an ON OVERFLOW clause:
    IF( t_overflow, ne_op, integer_zero_node )
    // And we have an overflow condition
    gg_append_statement( overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( not_overflow )
    {
    // We have a NOT ON OVERFLOW clause:
    IF( t_overflow, eq_op, integer_zero_node )
    // And there isn't an overflow condition:
    gg_append_statement( not_overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( overflow )
    {
    gg_append_statement( overflow->structs.unstring->bottom.label );
    free( overflow->structs.unstring );
    }

  if( not_overflow )
    {
    gg_append_statement( not_overflow->structs.unstring->bottom.label );
    free( not_overflow->structs.unstring );
    }
  }

void
parser_string(const cbl_refer_t& tgt,
              const cbl_refer_t& pointer,
              size_t nsource,
              cbl_string_src_t *sources,
              cbl_label_t *overflow,
              cbl_label_t *not_overflow )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_END
    }
  if( overflow )
    {
    gg_append_statement(overflow->structs.unstring->over.label);
    }
  if( not_overflow )
    {
    gg_append_statement(not_overflow->structs.unstring->over.label);
    }

  // We need an array of nsource+1 integers:
  size_t *integers = static_cast<size_t *>(xmalloc((nsource+1)*sizeof(size_t)));
  gcc_assert(integers);

  // Count up how many referlets we are going to need:
  size_t cblc_count = 2;  // tgt and pointer
  for(size_t i=0; i<nsource; i++)
    {
    cblc_count += 1 + sources[i].ninput; // 1 for identifier_2 + ninput identifier_1 values;
    }

  std::vector<cbl_refer_t> refers(cblc_count);

  size_t index_int = 0;
  size_t index_cblc = 0;

  integers[index_int++] = nsource;

  refers[index_cblc++] = tgt;
  refers[index_cblc++] = pointer;

  for(size_t i=0; i<nsource; i++)
    {
    integers[index_int++] = sources[i].ninput;
    refers[index_cblc++] = sources[i].delimited_by;
    for(size_t j=0; j<sources[i].ninput; j++)
      {
      refers[index_cblc++] = sources[i].inputs[j];
      }
    }

  gcc_assert(index_int == nsource+1);
  gcc_assert(index_cblc == cblc_count);

  tree pintegers = build_array_of_size_t( index_int, integers);
  tree referlets = build_array_of_referlets(index_cblc, refers.data());

  tree t_overflow = gg_define_variable(INT);
  gg_assign(t_overflow, gg_call_expr( INT,
                                      "__gg__string",
                                      pintegers,
                                      referlets,
                                      NULL_TREE));
  gg_free(pintegers);

  free(integers);

  if( overflow )
    {
    // We have an ON OVERFLOW clause:
    IF( t_overflow, ne_op, integer_zero_node )
    // And we have an overflow condition
    gg_append_statement( overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( not_overflow )
    {
    // We have a NOT ON OVERFLOW clause:
    IF( t_overflow, eq_op, integer_zero_node )
    // And there isn't an overflow condition:
    gg_append_statement( not_overflow->structs.unstring->into.go_to );
    ELSE
    ENDIF
    }

  if( overflow )
    {
    gg_append_statement( overflow->structs.unstring->bottom.label );
    free( overflow->structs.unstring );
    }

  if( not_overflow )
    {
    gg_append_statement( not_overflow->structs.unstring->bottom.label );
    free( not_overflow->structs.unstring );
    }
  }

void
parser_call_exception( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(name->name)
    SHOW_PARSE_END
    }

  name->structs.call_exception
    = static_cast<cbl_call_exception_t *>
                                (xmalloc(sizeof(struct cbl_call_exception_t)));
  gcc_assert(name->structs.call_exception);
  // Set up the address pairs for this clause
  gg_create_goto_pair(&name->structs.call_exception->over.go_to,
                      &name->structs.call_exception->over.label);
  gg_create_goto_pair(&name->structs.call_exception->into.go_to,
                      &name->structs.call_exception->into.label);
  gg_create_goto_pair(&name->structs.call_exception->bottom.go_to,
                      &name->structs.call_exception->bottom.label);

  // Jump over the [NOT] ON EXCEPTION code that is about to be laid down
  // char ach[128];
  // sprintf(ach, "# parser_call_exception %s: over.goto", name->name);
  // gg_insert_into_assembler(ach);
  SHOW_PARSE
    {
    SHOW_PARSE_INDENT
    SHOW_PARSE_TEXT("except over.goto")
    SHOW_PARSE_END
    }
  gg_append_statement( name->structs.call_exception->over.go_to );

  // Create the label that allows the following code to be executed at
  // the appropriate time.
  // sprintf(ach, "# parser_call_exception %s: into.label", name->name);
  // gg_insert_into_assembler(ach);
  gg_append_statement( name->structs.call_exception->into.label );
  }

void
parser_call_exception_end( cbl_label_t *name )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(name->name)
    SHOW_PARSE_END
    }
  // char ach[128];
  // sprintf(ach, "# parser_call_exception_end %s: bottom.goto", name->name);
  // gg_insert_into_assembler(ach);
  gg_append_statement( name->structs.call_exception->bottom.go_to );
  }

static
void
create_and_call(size_t narg,
                cbl_ffi_arg_t args[],
                tree function_pointer,
                const char *funcname,
                tree returned_value_type,
                cbl_refer_t returned,
                cbl_label_t *not_except)
  {
  // We have a good function handle, so we are going to create a call
  tree *arguments = NULL;
  int  *allocated = NULL;

  if(narg)
    {
    arguments = static_cast<tree *>(xmalloc(2*narg * sizeof(tree)));
    gcc_assert(arguments);
    allocated = static_cast<int *>(xmalloc(narg * sizeof(int)));
    gcc_assert(allocated);
    }

  // Put the arguments onto the "stack" of calling parameters:
  for( size_t i=0; i<narg; i++ )
    {
    cbl_ffi_crv_t crv = args[i].crv;

    if( args[i].refer.field && args[i].refer.field->type == FldLiteralN )
      {
      // Literals have to be passed by value
      crv = by_value_e;
      }

    if(    args[i].attr == address_of_e
        || args[i].attr == length_of_e
        || args[i].refer.addr_of )
      {
      // These have to be passed to be passed by value.
      crv = by_value_e;
      }
    else if(   crv == by_value_e
            && args[i].refer.field->type == FldAlphanumeric
            && (args[i].refer.field->attr & FIGCONST_MASK) != zero_value_e )
      {
      // Maybe passing an alphanumeric BY VALUE should be a syntax error?
      crv = by_content_e;
      }

    allocated[i] = 0;

    tree location = gg_define_variable(UCHAR_P);
    tree length   = gg_define_variable(SIZE_T);

    if( !args[i].refer.field )
      {
      // The PARAMETER is OMITTED
      arguments[i] = null_pointer_node;
      gg_assign(gg_array_value(var_decl_call_parameter_lengths, i),
                size_t_zero_node);
      continue;
      }

    if( refer_is_clean(args[i].refer) )
      {
      if( args[i].refer.field->type == FldLiteralA )
        {
        crv = by_content_e;
        gg_assign(location,
                  gg_cast(UCHAR_P,
                     build_string_literal(args[i].refer.field->data.capacity(),
                                       args[i].refer.field->data.original())));
        gg_assign(length,
                  build_int_cst_type( SIZE_T,
                                      args[i].refer.field->data.capacity()));
        }
      else
        {
        gg_assign(location,
                  member(args[i].refer.field->var_decl_node, "data"));
        gg_assign(length,
                  member(args[i].refer.field->var_decl_node, "capacity"));
        }
      }
    else
      {
      gg_assign(location,
                qualified_data_location(args[i].refer)),
      gg_assign(length,
                refer_size_source(args[i].refer));
      }

    switch( crv )
      {
      case by_default_e:
        gcc_unreachable();
        break;

      case by_reference_e:
        {
        arguments[i] = location;

        // Pass the pointer to the data location, so that the called program
        // can both access and change the data.
        break;
        }

      case by_content_e:
        {
        // BY CONTENT means that the called program gets a copy of the data.
        // We'll free this copy after the called program returns.

        switch(args[i].attr)
          {
          case address_of_e:
          case length_of_e:
            {
            // Up above, we converted these to by_value_e
            gcc_unreachable();
            break;
            }

          case none_of_e:
            {
            // Allocate the memory, and make the copy:
            arguments[i] = gg_define_variable(CHAR_P);
            allocated[i] = 1;
            gg_assign(arguments[i], gg_cast(CHAR_P, gg_malloc(length))) ;
            gg_memcpy(arguments[i], location, length);
            break;
            }
          }
        break;
        }

      case by_value_e:
        {
        // For BY VALUE, we take whatever we've been given and make a INT128
        // out of it if necessary, and either LONG or ULONG otherwise.

        cbl_ffi_arg_attr_t attr = args[i].attr;
        if( args[i].refer.addr_of )
          {
          attr = address_of_e;
          }

        switch(attr)
          {
          case address_of_e:
            {
            arguments[i] = gg_define_variable(SIZE_T);
            gg_assign(arguments[i], gg_cast(SIZE_T, location ));
            gg_assign(length, build_int_cst_type(SIZE_T, gg_sizeof(CHAR_P)));
            break;
            }

          case length_of_e:
            {
            arguments[i] = gg_define_variable(SIZE_T);
            gg_assign(arguments[i], gg_cast(SIZE_T, length));
            gg_assign(length, build_int_cst_type(SIZE_T, gg_sizeof(CHAR_P)));
            break;
            }

          case none_of_e:
            {
            tree type = tree_type_from_refer(args[i].refer);
            arguments[i] = gg_define_variable(type);
            safe_assign(arguments[i], args[i].refer);
            break;
            }
          }
        }
      }
    // The elements in this array tell the called routine the length of each
    // variable.  This value is used both to handle ANY LENGTH formal
    // parameters, and to provide information to the called program when being
    // passed expressions BY VALUE and BY CONTENT
    gg_assign(gg_array_value(var_decl_call_parameter_lengths, i), length);
    }

  // Let the called program know how many parameters we are passing
  gg_assign(var_decl_call_parameter_count,
            build_int_cst_type(INT, narg));

  tree call_expr = NULL_TREE;

  if( function_pointer )
    {
    gg_assign(var_decl_call_parameter_signature,
              gg_cast(CHAR_P, function_pointer));

    call_expr = gg_call_expr_list(returned_value_type,
                                  function_pointer,
                                  narg,
                                  arguments );
    }
  else
    {
    tree fndecl_type = build_varargs_function_type_array( returned_value_type,
                       0,     // No parameters yet
                       NULL); // And, hence, no types

    // Fetch the FUNCTION_DECL for that FUNCTION_TYPE
    tree function_decl = gg_build_fn_decl(funcname, fndecl_type);
    set_call_convention(function_decl, current_call_convention());
    // Take the address of the function decl:
    tree address_of_function = gg_get_address_of(function_decl);

    // Stash that address as the called program's signature:
    tree address_as_char_p = gg_cast(CHAR_P, address_of_function);
    tree assigment = gg_assign( var_decl_call_parameter_signature,
                                address_as_char_p);
    // The source of the assigment is the second element of a MODIFY_EXPR
    parser_call_target( funcname, assigment );

    // Create the call_expr from that address
    call_expr = build_call_array_loc( gg_token_location(),
                                      returned_value_type,
                                      address_of_function,
                                      narg,
                                      arguments);
    // Among other possibilities, this might be a forward reference to a
    // contained function.  The name  here is "prog2", and ultimately will need
    // to be replaced with a call to "prog2.62".  So, this call expr goes into
    // a list of call expressions whose function_decl targets will be replaced.
    parser_call_target( funcname, call_expr );
    }

  tree returned_value;

  if( returned.field )
    {
    // We expect the return value to be a 64-bit or 128-bit integer.  How
    // we treat that returned value depends on the target.

    // Create a variable of the type expected from the called function
    returned_value = gg_define_variable(returned_value_type);

    // Actually call the function, assigning the returned value to that
    // variable:
    push_program_state();
    gg_assign(returned_value, gg_cast(returned_value_type, call_expr));
    pop_program_state();

    // Now we decided what to do with the returned value, based on its type.
    if( returned_value_type == CHAR_P )
      {
      // Let the library do the assignment of the 'char *returned_value' to the
      // target 'refer returned'
      gg_call(VOID,
              "__gg__refer_from_psz",
              gg_get_address_of(returned.field->var_decl_node),
              refer_offset(returned),
              refer_size_dest(returned),
              returned_value,
              NULL_TREE);
      TRACE1
        {
        TRACE1_HEADER
        TRACE1_REFER("returned value: ", returned, "")
        TRACE1_END
        }
      }
    else if(    returned_value_type == SSIZE_T
            ||  returned_value_type == SIZE_T
            ||  returned_value_type == INT128
            ||  returned_value_type == UINT128)
      {
      // We got back a 64-bit or 128-bit integer.  The called and calling
      // programs have to agree on size, but other than that, integer numeric
      // types are converted one to the other.

      gg_call(VOID,
              "__gg__int128_to_qualified_field",
              gg_get_address_of(returned.field->var_decl_node),
              refer_offset(returned),
              refer_size_dest(returned),
              gg_cast(INT128, returned_value),
              gg_cast(INT, member(returned.field->var_decl_node, "rdigits")),
              build_int_cst_type(INT, truncation_e),
              null_pointer_node,
              NULL_TREE );
      TRACE1
        {
        TRACE1_HEADER
        TRACE1_REFER("returned value: ", returned, "")
        TRACE1_END
        }
      }
    else if(    returned_value_type == FLOAT
            ||  returned_value_type == DOUBLE
            ||  returned_value_type == FLOAT128)
      {
      tree returned_location = gg_define_variable(UCHAR_P);
      tree returned_length   = gg_define_variable(SIZE_T);
      // we were given a returned::field, so find its location and length:
      gg_assign(returned_location,
                qualified_data_location(returned));
      gg_assign(returned_length,
                refer_size_source(returned));

      // We are doing float-to-float, and we require that those be identical
      // one the caller and callee sides.
      gg_memcpy(  returned_location,
                  gg_get_address_of(returned_value),
                  returned_length);

      TRACE1
        {
        TRACE1_HEADER
        TRACE1_REFER("returned value: ", returned, "")
        TRACE1_END
        }
      }
    else
      {
      // Getting here should be impossible; it means we didn't anticipate
      // the type of the returned value:
      cbl_internal_error(
            "%s: What in the name of Nero are we doing here?",
            __func__);
      }
    }
  else
    {
    // There is no explicit location to assign the returned value.
    push_program_state();
    if( dialect_ibm() || dialect_mf() || dialect_gnu() )
      {
      // Because no explicit returning value is expected, we call the
      // designated function and assign the return value to our RETURN-CODE.
      gg_assign(current_function->var_decl_return, gg_cast(SHORT, call_expr));
      }
    else
      {
      // Because it is not IBM/MF/GNU, we execute the called function and
      // ignore any returned value.
      gg_append_statement(call_expr);
      }
    pop_program_state();
    }

  for( size_t i=0; i<narg; i++ )
    {
    if( allocated[i] )
      {
      gg_free(arguments[i]);
      }
    }
  free(arguments);
  free(allocated);

  if( not_except )
    {
    // We have an ON EXCEPT clause:
    gg_append_statement( not_except->structs.call_exception->into.go_to );
    }
  }

void
parser_call(   cbl_refer_t name,
               cbl_refer_t returned,  // This is set by RETURNING clause
               size_t narg,
               cbl_ffi_arg_t args[],
               cbl_label_t *except,
               cbl_label_t *not_except,
               bool /*is_function*/)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD( " calling ", name.field)
    if( except )
      {
      SHOW_PARSE_TEXT(" - except is ")
      SHOW_PARSE_TEXT(except->name)
      }
    if( not_except )
      {
      SHOW_PARSE_TEXT(" - not_except is ")
      SHOW_PARSE_TEXT(not_except->name)
      }
    SHOW_PARSE_TEXT(" (")
    for(size_t i=0; i<narg; i++)
      {
      const cbl_field_t *p = args[i].refer.field;
      SHOW_PARSE_FIELD( " ",  p)
      }
    SHOW_PARSE_TEXT(" )")
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_REFER("calling ", name, "");
    for(size_t i=0; i<narg; i++)
      {
      TRACE1_INDENT
      gg_fprintf(trace_handle, 1, "parameter %d: ", build_int_cst_type(INT, i+1));
      switch( args[i].crv )
        {
        case by_default_e: gcc_unreachable();
        case by_reference_e:
          TRACE1_TEXT(" BY REFERENCE ")
          break;
        case by_content_e:
          TRACE1_TEXT(" BY CONTENT ")
          break;
        case by_value_e:
          TRACE1_TEXT(" BY VALUE ")
          break;
        }
      TRACE1_REFER("", args[i].refer, "")
      }
    TRACE1_END
    }

  // If we have an ON EXCEPTION clause, a GOTO was established in
  // parser_call_exception().
  // Here is where we place the label for that GOTO

  if( except )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("except over.label:")
      }
    gg_append_statement(except->structs.call_exception->over.label);
    }

  // Likewise, for a NOT ON EXCEPTION
  if( not_except )
    {
    SHOW_PARSE
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_TEXT("not_except over.label:")
      }
    gg_append_statement(not_except->structs.call_exception->over.label);
    }

  // We are getting close to establishing the function_type.  To do that,
  // we want to establish the function's return type.

  size_t nbytes;
  tree returned_value_type = tree_type_from_field_type(returned.field, nbytes);

  if( use_static_call() && is_literal(name.field) )
    {
    // name is a literal
    create_and_call(narg,
                    args,
                    NULL_TREE,
                    name.field->data.original(),
                    returned_value_type,
                    returned,
                    not_except);
    }
  else if( name.field && name.field->type == FldPointer )
    {
    tree function_pointer = function_pointer_from_name( name,
                                                        returned_value_type);
    // This is call-by-pointer; we know function_pointer is good:
    create_and_call(narg,
                    args,
                    function_pointer,
                    nullptr,
                    returned_value_type,
                    returned,
                    not_except);
    }
  else
    {
    tree function_pointer = function_pointer_from_name( name,
                                                      returned_value_type);
    // We might not have a good handle, so we have to check:
    IF( function_pointer,
        ne_op,
        gg_cast(TREE_TYPE(function_pointer), null_pointer_node) )
      {
      create_and_call(narg,
                      args,
                      function_pointer,
                      nullptr,
                      returned_value_type,
                      returned,
                      not_except);
      }
    ELSE
      {
      // We have a bad function pointer, which is the exception condition:
      // Set the exception message to "name"
      gg_call(VOID,
              "__gg__set_exception_call",
              gg_get_address_of(name.field->var_decl_node),
              refer_offset(name),
              NULL_TREE);
      parser_exception_raise(ec_program_not_found_e);
      if( except )
        {
        // We have an ON EXCEPT clause:
        gg_append_statement( except->structs.call_exception->into.go_to );
        // Because there is an ON EXCEPTION clause, suppress DECLARATIVE
        // processing
        gg_assign(var_decl_exception_code, integer_zero_node);
        }
      else
        {
        // When EC-PROGRAM-NOT-FOUND is not enabled, we issue a warning.
        const cbl_enabled_exceptions_t&
                                enabled_exceptions( cdf_enabled_exceptions() );
        if( !enabled_exceptions.match(ec_program_not_found_e) )
          {
          tree mangled_name = gg_define_variable(CHAR_P);

          gg_call(VOID,
                  "__gg__just_mangle_name",
                  (name.field->var_decl_node
                                  ? gg_get_address_of(name.field->var_decl_node)
                                  : null_pointer_node),
                  gg_get_address_of(  mangled_name),
                  NULL_TREE);

          gg_printf("WARNING: %s:%d \"CALL %s\" not found"
                    " with no \"CALL ON EXCEPTION\" phrase.\n"
                    "(You might need -rdynamic or --export-dynamic for symbols in the executable.)\n",
                    gg_string_literal(current_filename.back().c_str()),
                    build_int_cst_type(INT, CURRENT_LINE_NUMBER),
                    mangled_name,
                    NULL_TREE);
          }
        }
      }
    ENDIF
    }

  // Clean up the label bookkeeping
  if( except )
    {
    gg_append_statement( except->structs.call_exception->bottom.label );
    free( except->structs.call_exception );
    }
  if( not_except )
    {
    gg_append_statement( not_except->structs.call_exception->bottom.label );
    free( not_except->structs.call_exception );
    }
  }

// Set global variable to use alternative ENTRY point.
void
parser_entry_activate( size_t iprog, const cbl_label_t *declarative )
  {
  assert(iprog == symbol_elem_of(declarative)->program);
  }

void
parser_entry( const cbl_field_t *name, size_t nusing, cbl_ffi_arg_t *args )
  {
  // We are implementing the ENTRY statement, which creates an alternative
  // entry point into the current program-id.  There is no actual way to do
  // that literally.  So, we are going to create a separate routine that sets
  // things up and then calls the current routine with the information it needs
  // to transfer processing to the ENTRY point.

  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" ")
    SHOW_PARSE_TEXT(name->data.original())
    SHOW_PARSE_END
    }

  // Get the name of the program that contains the ENTRY statement.
  char *name_of_parent = xstrdup(current_function->our_name);

  // Get the name of the ENTRY point.
  // cppcheck-suppress nullPointerRedundantCheck
  char *psz = cobol_name_mangler(name->data.original());

  // Create a goto/label pair.  The label will be set up here; the goto will
  // be used when we re-enter the containing function:

  tree entry_goto;
  tree entry_label;

  gg_create_goto_pair(&entry_goto,
                      &entry_label);

  size_t entry_index = current_function->entry_goto_expressions.size()+1;
  current_function->entry_goto_expressions.push_back(entry_goto);

  // Start creating the ENTRY function.
  tree function_decl = gg_define_function( VOID,
                                           psz,
                                           psz,
                                           NULL_TREE);
  free(psz);

  // Modify the default settings for this entry point
    TREE_ADDRESSABLE(function_decl) = 0;
    TREE_USED(function_decl) = 0;
    TREE_NOTHROW(function_decl) = 0;
    TREE_STATIC(function_decl) = 1;
    DECL_EXTERNAL (function_decl) = 0;
    TREE_PUBLIC (function_decl) = 1;
    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(function_decl) = 1;

  // When the ENTRY function point is called, we process its "using"
  // parameters:
  establish_using(nusing, args);

  // Put the entry_label into the global variable that will be picked up
  // when the containing program-id is re-entered:
  gg_assign(var_decl_entry_index, build_int_cst_type(SIZE_T, entry_index));

  // Get the function address of the containing function.
  tree gfa = gg_get_function_address(VOID, name_of_parent);
  free(name_of_parent);

  // Call the containing function
  gg_append_statement(gg_call_expr_list(VOID,
                                        gfa,
                                        0,
                                        NULL));
  // We are done with the ENTRY function:
  gg_finalize_function();

  // Lay down the address of the label that matches var_decl_entry_index;
  // the containing program-id will jump to this point.
  gg_append_statement(entry_label);
  }

void
parser_bitop( struct cbl_field_t *tgt,  // tgt has to be a FldConditional
              struct cbl_field_t *a,    // is modified by SET,CLEAR
              enum bitop_t op,
              size_t bitmask )
  {
  Analyze();
  // This routine is designed to set, clear, and test BITMASK bits in the
  // A operand.  For ON and OFF, it sets tgt, a FldConditional, to TRUE or FALSE

  // This is clumsy:  The ops[] array has to match bitop_t
  static const char *ops[] = { "SET", "CLEAR", "ON", "OFF",
                               "AND", "OR",    "XOR" };
  gcc_assert( op < COUNT_OF(ops) );
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD( " switch: ", a)
    fprintf(stderr, " mask: " HOST_SIZE_T_PRINT_HEX_PURE, (fmt_size_t)bitmask);
    fprintf(stderr, " op: %s", ops[op]);
    SHOW_PARSE_FIELD( " target ", tgt)
    SHOW_PARSE_END
    }

  if(tgt && tgt->type != FldConditional)
    {
    fprintf(stderr,
            "%s: The target %s has to be a FldConditional, not %s\n",
            __func__,
            tgt->name,
            cbl_field_type_str(tgt->type));
    gcc_unreachable();
    }

  switch(op)
    {
    case bit_set_op:
    case bit_clear_op:
      // For set_on and set_off operations, the tgt is superfluous, so I
      // did this code just in case the parser doesn't give us anything
      // to set
      gg_call(BOOL,
              "__gg__bitop",
              gg_get_address_of(a->var_decl_node),
              build_int_cst_type(INT, op),
              build_int_cst_type(SIZE_T, bitmask),
              NULL_TREE );
      break;

    case bit_on_op:
    case bit_off_op:
      gg_assign(  tgt->var_decl_node,
                  gg_call_expr(   BOOL,
                                  "__gg__bitop",
                                  gg_get_address_of(a->var_decl_node),
                                  build_int_cst_type(INT, op),
                                  build_int_cst_type(SIZE_T, bitmask),
                                  NULL_TREE));
      break;

    case bit_and_op:
    case bit_or_op:
    case bit_xor_op:
      fprintf(stderr,
              "%s: The %s operation is not valid\n",
              __func__,
              ops[op]);
      gcc_unreachable();
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    //TRACE1_FIELD_INFO( " target ", tgt)
    TRACE1_FIELD_INFO( " a ", a)
    TRACE1_END
    }
  }

void
parser_bitwise_op(struct cbl_field_t *tgt,
                  struct cbl_field_t *a,
                  enum bitop_t op,
                  size_t bitmask )
  {
  Analyze();
  // This routine is a specialized TGT = A op (size_t) bitmask, where OP is
  // AND, OR, or XOR.  A should be an integer type. tgt should be a valid target
  // for a move where an integer is the sender.

  // SET and CLEAR are straightforward.  ON returns true if any bitmask bit is
  // one in 'A'.  OFF returns true if any bitmask bit in 'A' is zero.

  // This is clumsy:  The ops[] array has to match bitop_t
  static const char *ops[] = { "SET", "CLEAR", "ON", "OFF",
                               "AND", "OR",    "XOR" };
  gcc_assert( op < COUNT_OF(ops) );
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD( " switch: ", a)
    fprintf(stderr, " mask: " HOST_SIZE_T_PRINT_HEX_PURE, (fmt_size_t)bitmask);
    fprintf(stderr, " op: %s", ops[op]);
    SHOW_PARSE_FIELD( " target ", tgt)
    SHOW_PARSE_END
    }

  if( tgt && !is_valuable(tgt->type) && tgt->type != FldLiteralN)
    {
    fprintf(stderr,
            "%s: The target %s has to be is_valuable, not %s\n",
            __func__,
            tgt->name,
            cbl_field_type_str(tgt->type));
    gcc_unreachable();
    }

  switch(op)
    {
    case bit_set_op:
    case bit_clear_op:
    case bit_on_op:
    case bit_off_op:
      fprintf(stderr,
              "%s: The %s operation is not valid\n",
              __func__,
              ops[op]);
      gcc_unreachable();
      break;

    case bit_and_op:
    case bit_or_op:
    case bit_xor_op:
      gg_call(VOID,
              "__gg__bitwise_op",
              gg_get_address_of(tgt->var_decl_node),
              gg_get_address_of(a->var_decl_node),
              build_int_cst_type(INT, op),
              build_int_cst_type(SIZE_T, bitmask),
              NULL_TREE );
      break;
    }

  TRACE1
    {
    TRACE1_HEADER
    //TRACE1_FIELD_INFO( " target ", tgt)
    TRACE1_FIELD_INFO( " a ", a)
    TRACE1_END
    }
  }

void
parser_set_pointers( size_t ntgt, cbl_refer_t *tgts, cbl_refer_t source )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" source ", source.field);
    char ach[128];
    sprintf(ach,
            " source.addr_of %s",
            source.addr_of ? "TRUE" : "FALSE" );
    SHOW_PARSE_TEXT(ach);
    for( size_t i=0; i<ntgt; i++ )
      {
      SHOW_PARSE_INDENT
      SHOW_PARSE_FIELD("target ", tgts[i].field)
      }
    SHOW_PARSE_END
    }
  for( size_t i=0; i<ntgt; i++ )
    {
    if(    !source.addr_of
        && (source.field->type == FldAlphanumeric
            || source.field->type == FldLiteralA))
      {
      // This is something like SET varp TO ENTRY "ref".
      tree function_pointer = function_pointer_from_name(source,
                                                   COBOL_FUNCTION_RETURN_TYPE);
      gg_memcpy(qualified_data_location(tgts[i]),
                gg_get_address_of(function_pointer),
                sizeof_pointer);
      }
    else
      {
      if( !tgts[i].addr_of )
        {
        // When not ADDRESS OF TARGET, the variable must be a POINTER
        gcc_assert( tgts[i].field->type == FldPointer );
        }
      else
        {
        // When ADDRESS OF TARGET, the target must be linkage or based
        gcc_assert( tgts[i].field->attr & (linkage_e | based_e) );
        }

      gg_call(  VOID,
                "__gg__set_pointer",
                gg_get_address_of(tgts[i].field->var_decl_node),
                refer_offset(tgts[i]),
                build_int_cst_type(INT, tgts[i].addr_of  ? REFER_T_ADDRESS_OF : 0),
                source.field ? gg_get_address_of(source.field->var_decl_node) : null_pointer_node,
                refer_offset(source),
                build_int_cst_type(INT, source.addr_of  ? REFER_T_ADDRESS_OF : 0),
                NULL_TREE
                );

      if( tgts[i].addr_of )
        {
        // When SET ADDRESS OF TARGET TO ..., the library call sets
        // tgts[i].field->data.  We need to propagate the data+offset
        // through the level01 variable's children:
        propogate_linkage_offsets(tgts[i].field,
                                  member(tgts[i].field->var_decl_node, "data"));
        }
      }
    }
  }
typedef struct hier_node
  {
  size_t our_index;     // In the symbol table
  bool   common;
  struct hier_node *parent_node;
  char *name;
  std::vector<struct hier_node *>child_nodes;

  hier_node() :
    our_index(0),
    common(false),
    parent_node(nullptr),
    name(nullptr)
    {}
  } hier_node;

static hier_node *
find_hier_node( const std::unordered_map<size_t, hier_node *> &node_map,
                size_t program_index)
  {
  std::unordered_map<size_t, hier_node *>::const_iterator it =
        node_map.find(program_index);
  if( it == node_map.end() )
    {
    return NULL;
    }
  return it->second;
  }

static bool
sort_by_hier_name(const hier_node *a, const hier_node *b)
  {
  return strcmp(a->name, b->name) < 0;
  }

static void
find_uncles(const hier_node *node, std::vector<const hier_node *> &uncles)
  {
  const hier_node *parent = node->parent_node;
  if( parent )
    {
    for(size_t i=0; i<parent->child_nodes.size(); i++)
      {
      if( parent->child_nodes[i] != node )
        {
        if( parent->child_nodes[i]->common )
          {
          uncles.push_back(parent->child_nodes[i]);
          }
        }
      }
    find_uncles(parent, uncles);
    }
  }

void
parser_program_hierarchy( const cbl_prog_hier_t& hier )
  {
  Analyze();
  /*  This routine gets called near the end of every program-id.  It keeps
      growing because the parser doesn't know when it is working on the last
      program of a list of nested programs.  So, we just do what we need to do,
      and we keep track of what we've already built so that we don't build it
      more than once.
      */
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if( gg_trans_unit.function_stack.size() != 1 )
      {
      SHOW_PARSE_TEXT("Ending a nested function")
      }
    else
      {
      for( size_t i=0; i<hier.labels.size(); i++ )
        {
        if( i )
          {
          SHOW_PARSE_INDENT
          }
        else
          {
          SHOW_PARSE_TEXT(" ");
          }
        char ach[128];
        sprintf(ach,
                HOST_SIZE_T_PRINT_DEC " %s%s parent:" HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)hier.labels[i].ordinal,
                hier.labels[i].label.name,
                hier.labels[i].label.common ? " COMMON" : "",
                (fmt_size_t)hier.labels[i].label.parent);
        SHOW_PARSE_TEXT(ach);
        }
      }
    SHOW_PARSE_END
    }

  RETURN_WHEN_HIJACKED;

  // This needs to be an island that doesn't execute in-line.  This is necessary
  // when there isn't a GOBACK or GOTO or STOP RUN at the point where a
  // [possibly implicit] PROGRAM END is encountered
  tree skipper_goto;
  tree skipper_label;
  gg_create_goto_pair(&skipper_goto,
                      &skipper_label);
  gg_append_statement(skipper_goto);

  // The stack.size() test shouldn't be necessary, because the parser should
  // be calling us only at the PROGRAM END point of an outermost function.

  gcc_assert(gg_trans_unit.function_stack.size() == 1);

  gg_append_statement(label_list_out_label);

  std::unordered_map<size_t, std::vector<const hier_node *>> map_of_lists;
  std::unordered_map<size_t, hier_node *> node_map;
  std::vector<hier_node *> nodes;

  // We need to avoid duplicating names, because a direct child's name takes
  // precedence over a COMMON name above us in the hierarchy:

  std::unordered_map<size_t, std::unordered_set<std::string>>map_of_sets;

  // We need to build a tree out of the hierarchical structure:
  // Create, essentially, a root node:
  hier_node *zero_node = new hier_node;
  nodes.push_back(zero_node);
  node_map[0] = nodes.back();

  // Pass 1: Create a node for every program:
  for( size_t i=0; i<hier.labels.size(); i++ )
    {
    const hier_node *existing_node = find_hier_node(node_map, hier.labels[i].ordinal);
    gcc_assert( existing_node == NULL );

    hier_node *new_node = new hier_node;
    new_node->our_index    = hier.labels[i].ordinal;
    new_node->common       = hier.labels[i].label.common;
    new_node->name         = cobol_name_mangler(hier.labels[i].label.name);
    nodes.push_back(new_node);
    node_map[hier.labels[i].ordinal] = nodes.back();
    }

  // Pass 2: populate each node with their parent and children:
  for( size_t i=0; i<hier.labels.size(); i++ )
    {
    hier_node *child_node = find_hier_node(node_map, hier.labels[i].ordinal);
    gcc_assert(child_node);

    hier_node *parent_node = find_hier_node(node_map,
                                            hier.labels[i].label.parent);
    gcc_assert(parent_node);

    child_node->parent_node = parent_node;
    parent_node->child_nodes.push_back(child_node);
    }

  // We now build the lists of routines that can be called from every routine

  // We are going to create one vector of hier_nodes for each routine:

  for(size_t i=0; i<nodes.size(); i++)
    {
    // First, direct children always take precedence
    size_t caller = nodes[i]->our_index;
    const hier_node *caller_node = nodes[i];
    for(size_t j=0; j<caller_node->child_nodes.size(); j++)
      {
      map_of_lists[caller].push_back(caller_node->child_nodes[j]);
      map_of_sets[caller].insert(caller_node->child_nodes[j]->name);
      }

    // Sibling routines marked COMMON, and siblings of ancestors marked COMMON
    // are also accessible by us.  Go find them.
    std::vector<const hier_node *>uncles;
    find_uncles(nodes[i], uncles);
    for( size_t j=0; j<uncles.size(); j++ )
      {
      const hier_node *uncle = uncles[j];
      if( map_of_sets[caller].find(uncle->name) == map_of_sets[caller].end() )
        {
        // We have a COMMON uncle or sibling we haven't seen before.
        map_of_lists[caller].push_back(uncle);
        }
      }
    }

  // Having created lists of callables for each caller, we want to sort each
  // of those lists to make it easier to bsearch things in them later:
  for(  std::unordered_map<size_t, std::vector<const hier_node *>>::iterator mol = map_of_lists.begin();
        mol != map_of_lists.end();
        mol++ )
    {
    std::sort(mol->second.begin(), mol->second.end(), sort_by_hier_name);
    }

  // Having built the lists of lists, start pulling them apart

  tree function_type =
    build_varargs_function_type_array( SIZE_T,
                                       0,     // No parameters yet
                                       NULL); // And, hence, no types
  tree pointer_type = build_pointer_type(function_type);

  static std::unordered_set<size_t>callers;

  for(  std::unordered_map<size_t, std::vector<const hier_node *>>::const_iterator mol = map_of_lists.begin();
        mol != map_of_lists.end();
        mol++ )
    {
    size_t caller = mol->first;
    if( caller != 0 )
      {
      if( callers.find(caller) == callers.end() )
        {
        // We haven't seen this caller before

        char ach[3*sizeof(cbl_name_t)];
        tree names_table_type = build_array_type_nelts(CHAR_P, mol->second.size()+1);
        sprintf(ach, "..our_accessible_functions_" HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)caller);
        tree the_names_table = gg_define_variable(names_table_type, ach, vs_file_static);

        // Here is where we build a table out of constructors:
        tree constructed_array_type   = build_array_type_nelts(pointer_type, mol->second.size());
        sprintf(ach, "..our_constructed_table_" HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)caller);
        tree the_constructed_table = gg_define_variable(constructed_array_type, ach, vs_file_static);

        tree constr_names = make_node(CONSTRUCTOR);
        TREE_TYPE(constr_names) = names_table_type;
        TREE_STATIC(constr_names)    = 1;
        TREE_CONSTANT(constr_names)  = 1;

        tree constr = make_node(CONSTRUCTOR);
        TREE_TYPE(constr) = constructed_array_type;
        TREE_STATIC(constr)    = 1;
        TREE_CONSTANT(constr)  = 1;

        int i=0;
        for(  std::vector<const hier_node *>::const_iterator callee = mol->second.begin();
              callee != mol->second.end();
              callee++ )
          {
          sprintf(ach,
                  "%s." HOST_SIZE_T_PRINT_DEC,
                  (*callee)->name,
                  (fmt_size_t)(*callee)->parent_node->our_index);

          CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr_names),
                                  build_int_cst_type(SIZE_T, i),
                                  build_string_literal(ach));

          // Build the constructor element for that function:
          tree function_decl = build_fn_decl (ach, function_type);
          tree addr_expr = build1(ADDR_EXPR, pointer_type, function_decl);

          CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr),
                                  build_int_cst_type(SIZE_T, i),
                                  addr_expr);

          i++;
          }
        // Terminate the names table with NULL
        CONSTRUCTOR_APPEND_ELT( CONSTRUCTOR_ELTS(constr_names),
                                build_int_cst_type(SIZE_T, i),
                                null_pointer_node);

        DECL_INITIAL(the_names_table) = constr_names;
        DECL_INITIAL(the_constructed_table) = constr;

        // And put a pointer to that table into the file-static variable set aside
        // for it:
        sprintf(ach, "..accessible_program_list_" HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)caller);
        tree accessible_list_var_decl = gg_trans_unit_var_decl(ach);
        gg_assign( accessible_list_var_decl, gg_pointer_to_array(the_names_table) );

        sprintf(ach, "..accessible_program_pointers_" HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)caller);
        tree accessible_programs_decl = gg_trans_unit_var_decl(ach);
        gg_assign( accessible_programs_decl, gg_pointer_to_array(the_constructed_table) );

        callers.insert(caller);
        }
      }
    }
  gg_append_statement(label_list_back_goto);
  gg_append_statement(skipper_label);
  }

void
parser_set_numeric(struct cbl_field_t *tgt, ssize_t value)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" set ")
    SHOW_PARSE_TEXT(tgt->name)
    SHOW_PARSE_TEXT(" to ")
    char ach[32];
    sprintf(ach, HOST_SIZE_T_PRINT_DEC, (fmt_size_t)value);
    SHOW_PARSE_TEXT(ach);
    SHOW_PARSE_END
    }

  gg_call(VOID,
          "__gg__int128_to_field",
          gg_get_address_of(tgt->var_decl_node),
          build_int_cst_type(INT128, value),
          integer_zero_node,
          build_int_cst_type(INT, truncation_e),
          null_pointer_node,
          NULL_TREE );
  }

void
parser_exception_clear()
  {
  if( mode_syntax_only() ) return;

  Analyze();
  gg_assign(var_decl_exception_code, integer_zero_node);
  }

void
parser_exception_raise(ec_type_t ec)
  {
  Analyze();
  if( ec == ec_none_e )
    {
    gg_call(VOID,
            "__gg__set_exception_code",
            integer_zero_node,
            integer_one_node,
            NULL_TREE);
    }
  else
    {
    set_exception_code_func(ec, __LINE__, 1);
    }
  }

void
parser_match_exception(cbl_field_t *index)
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" index   ", index)
    SHOW_PARSE_INDENT
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_FIELD("index   ", index, "")
    TRACE1_INDENT
    TRACE1_END
    }

  gg_call(VOID,
          "__gg__match_exception",
          gg_get_address_of(index->var_decl_node),
          NULL_TREE);

  TRACE1
    {
    tree index_val;
    get_binary_value(index_val, index, INT);
    TRACE1_INDENT
    gg_printf("returned value is 0x%x (%d)", index_val, index_val, NULL_TREE);
    TRACE1_END
    }
  }

void
parser_check_fatal_exception()
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Check for fatal EC...")
    SHOW_PARSE_END
    }
  TRACE1
    {
    TRACE1_HEADER
    TRACE1_TEXT(" Check for fatal EC...")
    TRACE1_END
    }

  // Performance note:
  // A simple program that does two billion additions of 32-bit binary numbers
  // in its innermost loop had an execution time of 19.5 seconds.  By putting in
  // the if() statement, that was reduced to 3.8 seconds.

  if( cdf_enabled_exceptions().size() || sv_is_i_o )
    {
    gg_call(VOID,
            "__gg__check_fatal_exception",
            NULL_TREE);
    }
  }

void
parser_push_exception()
  {
  gg_call(VOID, "__gg__exception_push", NULL_TREE);
  }

void
parser_pop_exception()
  {
  gg_call(VOID, "__gg__exception_pop", NULL_TREE);
  }

void
parser_clear_exception()
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_TEXT(" Clear raised EC...")
    SHOW_PARSE_END
    }
  gg_call(VOID, "__gg__clear_exception", NULL_TREE);
  }

void
parser_exception_file( cbl_field_t *tgt, cbl_file_t *file)
  {
  Analyze();
  RETURN_IF_PARSE_ONLY;
  gg_call(VOID,
          "__gg__func_exception_file",
          gg_get_address_of(tgt->var_decl_node),
          file ? gg_get_address_of(file->var_decl_node) : null_pointer_node,
          NULL_TREE);
  }

void
parser_file_stash( struct cbl_file_t *file )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    if(file)
      {
      SHOW_PARSE_TEXT(" ");
      SHOW_PARSE_TEXT(file->name);
      }
    else
      {
      SHOW_PARSE_TEXT(" *file is NULL ")
      }
    SHOW_PARSE_END
    }

  if( file )
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("parser_file_stash of ")
      TRACE1_TEXT(file->name);
      TRACE1_END
      }

    gg_call(VOID,
            "__gg__file_stash",
            gg_get_address_of(file->var_decl_node),
            NULL_TREE);
    }
  else
    {
    TRACE1
      {
      TRACE1_HEADER
      TRACE1_TEXT("parser_file_stash of NULL ")
      TRACE1_END
      }

    gg_call(VOID,
            "__gg__file_stash",
            null_pointer_node,
            NULL_TREE);
    }
  }

#ifdef ENABLE_HIJACKING
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
static tree
build_temporaryN(int N)
  {
  //  Creates a typical FldNumericBin5 intermediate.
  char achName[32];
  sprintf(achName,"_funky_%d", N);
  char *pszdata = xasprintf("_funky%d_data", N);
  size_t bytes_to_allocate = 16;
  gg_variable_scope_t vs_scope = vs_stack;
  tree array_type = build_array_type_nelts(UCHAR, bytes_to_allocate);
  tree data_decl_node = gg_define_variable(
                      array_type,
                      pszdata,
                      vs_scope);
  free(pszdata);

  // This is the holy grail.  With the initializer set to gg_pointer_to_array,
  // we get N-squared behavior.  Set to null_pointer_node, linear.
  tree data_area = null_pointer_node;
  if( data_decl_node != null_pointer_node )
    {
    data_area = gg_pointer_to_array(data_decl_node);
    }

  char *psz = xasprintf("_funky%d", N);
  tree cobfield = gg_define_variable(cblc_field_type_node, psz, vs_stack);
  free(psz);

  tree data = data_area;        //  UCHAR_P, "data",
  tree capacity = build_int_cst_type(SIZE_T, 16);     //  SIZE_T,  "capacity",
  tree allocated = build_int_cst_type(SIZE_T, 16);    //  SIZE_T,  "allocated",
  tree offset = build_int_cst_type(SIZE_T, 0);       //  SIZE_T,  "offset",
  tree name = gg_string_literal(achName);         //  CHAR_P,  "name",
  tree picture = gg_string_literal("");      //  CHAR_P,  "picture",
  tree initial = null_pointer_node;      //  CHAR_P,  "initial",
  tree parent = null_pointer_node;       //  CHAR_P,  "parent",
  tree occurs_lower = build_int_cst_type(SIZE_T, 0); //  SIZE_T,  "occurs_lower",
  tree occurs_upper = build_int_cst_type(SIZE_T, 0); //  SIZE_T,  "occurs_upper");
  tree attr = build_int_cst_type(SIZE_T, intermediate_e);         //  SIZE_T,  "attr",
  tree type = build_int_cst_type(SCHAR, FldNumericBin5);         //  SCHAR,   "type",
  tree level = build_int_cst_type(SCHAR, 0);        //  SCHAR,   "level",
  tree digits = build_int_cst_type(SCHAR, 0);       //  SCHAR,   "digits",
  tree rdigits = build_int_cst_type(SCHAR, 0);      //  SCHAR,   "rdigits",
  tree tencoding = build_int_cst_type(INT, 111);    //  INT,     "encoding",
  tree alphabet = build_int_cst_type(INT, 0);    //  INT,     "alphabet",

  gg_structure_type_constructor(
      cobfield,
      data ,        //  UCHAR_P, "data",
      capacity,     //  SIZE_T,  "capacity",
      allocated,    //  SIZE_T,  "allocated",
      offset,       //  SIZE_T,  "offset",
      name,         //  CHAR_P,  "name",
      picture,      //  CHAR_P,  "picture",
      initial,      //  CHAR_P,  "initial",
      parent,       //  CHAR_P,  "parent",
      occurs_lower, //  SIZE_T,  "occurs_lower",
      occurs_upper, //  SIZE_T,  "occurs_upper");
      attr,         //  SIZE_T,  "attr",
      type,         //  SCHAR,   "type",
      level,        //  SCHAR,   "level",
      digits,       //  SCHAR,   "digits",
      rdigits,      //  SCHAR,   "rdigits",
      tencoding,    //  INT,     "encoding",
      alphabet);    //  INT,     "alphabet",

  return cobfield;
  }
#pragma GCC diagnostic pop

static void
hijack_for_development(const char *funcname)
  {
  static const int N = 10000;
  /* This routine is designed to allow the creation of a program-id program
     without requiring the parser to supply parser_xxx calls.

     When your source code is a "program-id. dubner.", this routine gets
     generated instead of the one in the source.
     */

  hijacked = true;
  funcname = "main";
  // Assume that funcname is lowercase with no hyphens
  gg_define_function(COBOL_FUNCTION_RETURN_TYPE,
                     funcname,
                     funcname,
                     NULL_TREE);

  parser_display_literal("You have been hijacked by a program named \"dubner_h\"");
  gg_insert_into_assemblerf("%s HIJACKED CODE START", ASM_COMMENT_START);


  tree xxx = gg_define_variable(INT, "xxx");
  tree yyy = gg_define_variable(INT, "yyy");
  tree zzz = gg_define_variable(INT, "zzz");

  fprintf(stderr, "N is %d\n", N);
  for(int i=0; i<N; i++)
    {
    IF( gg_bitwise_and(xxx, integer_one_node), ne_op, integer_zero_node )
      {
      gg_assign(yyy, xxx);
      }
    ELSE
      {
      gg_assign(zzz, xxx);
      }
    ENDIF
    }

  gg_insert_into_assemblerf("%s HIJACKED CODE END", ASM_COMMENT_START);
  }

static void
hijacker()
  {
  /* The code here is activated when the program-id is "hijack".  It's not
     really a hijacking; all of the code in the "hijack" program gets laid
     down.  The code here is injected just prior to the parser_exit() stuff
     in the COBOL source code. */

  parser_display_literal("You have been hijacked by a program named \"hijack_h\"");
  gg_insert_into_assemblerf("%s HIJACKED CODE START", ASM_COMMENT_START);

  tree foo = gg_define_variable(INT);
  IF( integer_one_node, eq_op, integer_one_node )
    {
    gg_printf("1 is indeed equal to 1\n", NULL_TREE);
    gg_assign(foo, build_int_cst_type(INT, 123));
    }
  ELSE
    {
    gg_printf("1 is NOT equal to 1!\n", NULL_TREE);
    gg_abort();
    gg_assign(foo, build_int_cst_type(INT, 999));
    }
  ENDIF
  gg_printf("\"foo\" is %d\n", foo, NULL_TREE);

#if 0
  // Leave this around for reference; it's how you find variables set up
  // in WORKING-STORAGE when involved in a hijack.
  cbl_field_t *faaa = register_find("aaa");
  cbl_field_t *fbbb = register_find("bbb");
  cbl_field_t *fddd = register_find("ddd");
  cbl_field_t *fxxx = register_find("xxx");

  cbl_refer_t aaa(faaa);
  cbl_refer_t bbb(fbbb);
  cbl_refer_t ddd(fddd);

  fxxx->var_decl_node = build_temporaryN(0);

  static const int N = 1000;
  fprintf(stderr, "N is %d\n", N);
  for(int i=0; i<N; i++)
    {
    parser_op(ddd,
              aaa,
              '+',
              bbb,
              NULL);
    }
#endif

  gg_insert_into_assemblerf("%s HIJACKED CODE END", ASM_COMMENT_START);
  }
#endif

tree parser_cast_long(tree N)
  {
  return gg_cast(LONG, N);
  }

void
parser_print_long(tree N)
  {
  gg_printf("%ld", N, NULL_TREE);
  }

void
parser_print_long(const char *fmt, tree N)
  {
  // fmt should have a %ld/%lx in it
  gg_printf(fmt, N, NULL_TREE);
  }

void
parser_print_long(long N)
  {
  gg_printf("%ld", build_int_cst_type(LONG, N), NULL_TREE);
  }

void
parser_print_long(const char *fmt, long N)
  {
  // fmt should have a %ld/%lx in it
  gg_printf(fmt, build_int_cst_type(LONG, N), NULL_TREE);
  }

void
parser_print_string(const char *ach)
  {
  gg_printf("%s", gg_string_literal(ach), NULL_TREE);
  }

void
parser_print_string(const char *fmt, const char *ach)
  {
  // fmt should have a %s in it
  gg_printf(fmt, gg_string_literal(ach), NULL_TREE);
  }

REAL_VALUE_TYPE
real_powi10 (uint32_t x)
{
  REAL_VALUE_TYPE ten, pow10;
  real_from_integer (&ten, TYPE_MODE (FLOAT128), 10, SIGNED);
  real_powi (&pow10, TYPE_MODE (FLOAT128), &ten, x);
  return pow10;
}

static tree
convert_data_initial(cbl_field_t * field)
  {
  // This routine returns a tree from field->data.initial, extended with
  // a NUL on the end.
  size_t buffer_size = field->data.capacity() + field->codeset.stride();
  char *buffer = static_cast<char *>(xmalloc(buffer_size));
  gcc_assert(buffer);

  size_t nbytes = field->data.capacity();

  const char *converted = field->data.initial;

  // Copy the converted bytes
  gcc_assert(nbytes < buffer_size);
  memcpy(buffer, converted, nbytes);
  charmap_t *charmap = __gg__get_charmap(field->codeset.encoding);

  // Tack on a final NUL
  charmap->putch(0, buffer, nbytes);

  tree retval = build_string_literal( buffer_size,
                                      buffer);
  free(buffer);
  return retval;
  }

static void
actually_create_the_static_field( cbl_field_t *new_var,
                                  tree data_area,
                                  size_t length_of_initial_string,
                                  const char *new_initial,
                                  tree immediate_parent,
                                  tree new_var_decl)
  {
  //  For FldLiteralN we force the encoding to be ASCII.
  //  See initial_from_initial() for an explanation.
  //  For FldClass, we force the encoding to be UTF32; see
  cbl_encoding_t encoding;
  if( new_var->type == FldLiteralN )
    {
    encoding = new_var->codeset.default_encodings.source->type;
    }
  else if( new_var->type == FldClass )
    {
    encoding = HOST_32_ENCODING;
    }
  else
    {
    encoding = new_var->codeset.encoding;
    }

  tree data = data_area ;
  tree capacity = build_int_cst_type( SIZE_T, new_var->data.capacity());
  tree allocated;
  if( data_area != null_pointer_node )
    {
    allocated = build_int_cst_type(SIZE_T, new_var->data.capacity());
    }
  else
    {
    allocated = build_int_cst_type(SIZE_T, 0) ;
    }
  tree offset = build_int_cst_type(SIZE_T, new_var->offset);
  tree name = gg_string_literal(new_var->name);
  tree picture = gg_string_literal(new_var->data.picture);
  tree initial;
  if( length_of_initial_string == 0 || !new_var->data.has_initial_value() )
    {
    initial = null_pointer_node;
    }
  else
    {
    initial = build_string_literal(length_of_initial_string, new_initial);
    }
  tree parent = immediate_parent ? gg_get_address_of(immediate_parent)
                                 : null_pointer_node ;
  tree occurs_lower = build_int_cst_type(SIZE_T, new_var->occurs.bounds.lower);
  tree occurs_upper = build_int_cst_type(SIZE_T, new_var->occurs.bounds.upper);
  tree attr = build_int_cst_type(SIZE_T, new_var->attr) ;
  tree type = build_int_cst_type(SCHAR, new_var->type) ;
  tree level = build_int_cst_type(SCHAR, new_var->level) ;
  tree digits = build_int_cst_type(SCHAR, new_var->data.digits) ;
  tree rdigits = build_int_cst_type(SCHAR, new_var->data.rdigits) ;
  tree tencoding = build_int_cst_type(INT, encoding);
  tree alphabet = build_int_cst_type(INT, new_var->codeset.alphabet);

  gg_structure_type_constructor(
      new_var_decl,
      data ,        //  UCHAR_P, "data",
      capacity,     //  SIZE_T,  "capacity",
      allocated,    //  SIZE_T,  "allocated",
      offset,       //  SIZE_T,  "offset",
      name,         //  CHAR_P,  "name",
      picture,      //  CHAR_P,  "picture",
      initial,      //  CHAR_P,  "initial",
      parent,       //  CHAR_P,  "parent",
      occurs_lower, //  SIZE_T,  "occurs_lower",
      occurs_upper, //  SIZE_T,  "occurs_upper");
      attr,         //  SIZE_T,  "attr",
      type,         //  SCHAR,   "type",
      level,        //  SCHAR,   "level",
      digits,       //  SCHAR,   "digits",
      rdigits,      //  SCHAR,   "rdigits",
      tencoding,    //  INT,     "encoding",
      alphabet);    //  INT,     "alphabet",
  }

static void
psa_global(cbl_field_t *new_var)
  {
  if( strcmp(new_var->name, "_VERY_TRUE") == 0 )
    {
    new_var->var_decl_node = boolean_true_node;
    return;
    }
  if( strcmp(new_var->name, "_VERY_FALSE") == 0 )
    {
    new_var->var_decl_node = boolean_false_node;
    return;
    }

  // global variables already have a cblc_field_t defined in constants.cc.

  // Finding their name is done by converting to lowercase, dashes become
  // underscores, and "__ggsr__" is prepended.  "filler" gets ignored.

  // To feed GDB-COBOL's requirements, we tack on this variable's index and
  // this program's index number:

  char ach[2*sizeof(cbl_name_t)];

  snprintf( ach,
            sizeof(ach),
            "__ggsr__%s",
            new_var->name);
  for(size_t i=0; i<strlen(ach); i++)
    {
    ach[i] = _tolower(ach[i]);
    if(ach[i] == '-')
      {
      ach[i] = '_';
      }
    }

  new_var->var_decl_node = gg_declare_variable(cblc_field_type_node, ach, NULL, vs_extern);

  // global variables already have a .data area defined.  We can find that
  // variable from the new_var->name.  It's lower-case, with hyphens
  // converted to underscores
  strcpy(ach, "__gg__data_");
  strcat(ach, new_var->name);
  for(size_t i=0; i<strlen(ach); i++)
    {
    ach[i] = _tolower(ach[i]);
    if(ach[i] == '-')
      {
      ach[i] = '_';
      }
    }
  new_var->data_decl_node = gg_declare_variable(UCHAR, ach, NULL, vs_extern);
  }

static tree
psa_new_var_decl(cbl_field_t *new_var, const char *external_record_base)
  {
  // This routine creates the VAR_DECL for the cblc_field_t that we are about
  // to statically create.
  tree new_var_decl;

  if( *external_record_base )
    {
    char ach[257];
    strcpy(ach, "_");
    strcat(ach, external_record_base);
    strcat(ach, "_ra");  // For "Record Area"
    new_var_decl = gg_define_variable(  cblc_field_type_node,
                                        ach,
                                        vs_weak);
    SET_DECL_MODE(new_var_decl, BLKmode);
    }
  else
    {
    size_t our_index = new_var->our_index;

    // During the early stages of implementing cbl_field_t::our_index, there
    // were execution paths in parse.y and parser.cc that resulted in our_index
    // not being set.  I hereby try to use field_index() to find the index
    // of this field to resolve those.  I note that field_index does a linear
    // search of the symbols[] table to find that index.  That's why I don't
    // use it routinely; it results in O(N^squared) computational complexity
    // to do a linear search of the symbol table for each symbol

    if(   !our_index
          && ! new_var->is_numeric_constant()
          && !(new_var->attr & intermediate_e))
      {
      our_index = field_index(new_var);
      if( our_index == (size_t)-1 )
        {
        // Hmm.  Couldn't find it.  Seems odd.
        our_index = 0;
        }
      }

    char base_name[257];
    char id_string[32] = "";

    if( new_var->attr & external_e )
      {
      // For external variables, just stick with the original name
      sprintf(base_name, "%s.cblc", new_var->name);
      }
    else
      {
      if(    our_index
          && new_var->parent
          && symbol_at(new_var->parent)->type == SymField )
        {
        // We have a parent that is a field
        sprintf(id_string, "." HOST_SIZE_T_PRINT_DEC "_" HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)our_index, (fmt_size_t)new_var->parent);
        }
      else
        {
        // The parent is zero, so it'll be implied:
        sprintf(id_string, "." HOST_SIZE_T_PRINT_DEC,
                (fmt_size_t)our_index);
        }

      if(strcasecmp(new_var->name, "filler") == 0)
        {
        // Multiple "fillers" can have the same parent, so we use filler_count
        // to distinguish them.  We also prepend an underscore, so that
        // the user can't trip us up by creating their *own* cobol variable
        // named "FILLER-1"
        static int filler_count = 1;
        sprintf(base_name, "_filler_%d", filler_count++);
        }
      else if( strlen(new_var->name) == 0 )
        {
        // This can happen.
        static int empty_count = 1;
        sprintf(base_name,
                "_%s_%d",
                cbl_field_type_str(new_var->type),
                empty_count++);
        }
      else if( new_var->attr & intermediate_e )
        {
        static int inter_count = 1;
        sprintf(base_name,
                "_%s_%s_%d",
                "intermediate",
                new_var->name,
                inter_count++);
        }
      else
        {
        strcpy(base_name, new_var->name);
        }
      strcat(base_name, id_string);
      }

    if( new_var->attr & external_e )
      {
      //fprintf(stderr, "external_e base name is %s\n", base_name);
      new_var_decl = gg_define_variable(  cblc_field_type_node,
                                          base_name,
                                          vs_weak);
      SET_DECL_MODE(new_var_decl, BLKmode);
      }
    else if( new_var->attr & (intermediate_e)
              && new_var->type != FldLiteralA
              && new_var->type != FldLiteralN )
      {
      gg_variable_scope_t scope = vs_stack;
      if( new_var->type == FldAlphanumeric )
        {
        // This has to be static, because we are putting the actual memory
        // on the heap.  But if we put the cblc_field_t on the stack inside
        // of a condition, or in a loop, we just keep recreating the field
        // without freeing the memory.  Eventually, with perhaps a
        // two-pass compiler, we'll be able to create the stack cblc_field_t
        // once per program-id.
        scope = vs_static;
        }
      new_var_decl = gg_define_variable(  cblc_field_type_node,
                                          base_name,
                                          scope);
      SET_DECL_MODE(new_var_decl, BLKmode);
      }
    else
      {
      new_var_decl = gg_define_variable(  cblc_field_type_node,
                                          base_name,
                                          vs_static);
      SET_DECL_MODE(new_var_decl, BLKmode);
      }
    }
  return new_var_decl;
  }

static void
psa_FldLiteralA(struct cbl_field_t *field )
  {
  Analyze();
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", field)
    SHOW_PARSE_END
    }

  TRACE1
    {
    TRACE1_HEADER
    TRACE1_END
    }

  // We are constructing a completely static constant structure.  We know the
  // capacity.  We'll create it from the data.initial.

  tree converted = convert_data_initial(field);

  static const char name_base[] = "_literal_a_";

  static int nvar = 0;
  nvar += 1;

  char ach[32];
  sprintf(ach, "%s%d", name_base, nvar);
  field->var_decl_node  = gg_define_variable( cblc_field_type_node,
                                              ach,
                                              vs_file_static);
  TREE_READONLY(field->var_decl_node) = 1;
  TREE_USED(field->var_decl_node) = 1;
  TREE_STATIC(field->var_decl_node) = 1;
  DECL_PRESERVE_P (field->var_decl_node) = 1;

  actually_create_the_static_field(
              field,
              converted,
              strlen(field->data.original())+1,
              field->data.original(),
              NULL_TREE,
              field->var_decl_node);
  }

void
parser_local_add(struct cbl_field_t *new_var )
  {
  SHOW_PARSE
    {
    SHOW_PARSE_HEADER
    SHOW_PARSE_FIELD(" ", new_var);
    SHOW_PARSE_END
    }

  CHECK_FIELD(new_var);

  IF( member(new_var->var_decl_node, "data"),
      ne_op,
      gg_cast(UCHAR_P, null_pointer_node) )
    {
    gg_call(VOID,
            "__gg__push_local_variable",
            gg_get_address_of(new_var->var_decl_node),
          NULL_TREE);
    }
  ELSE
    ENDIF

  if( new_var->level == LEVEL01 || new_var->level == LEVEL77)
    {
    // We need to allocate memory on the stack for this variable
    tree array_type = build_array_type_nelts(UCHAR, new_var->data.capacity());
    tree data_decl_node = gg_define_variable( array_type,
                                                    NULL,
                                                    vs_stack);
    gg_assign( member(new_var->var_decl_node, "data"),
                      gg_pointer_to_array(data_decl_node) );
    }
  cbl_refer_t wrapper;
  wrapper.field = new_var;
  initialize_variable_internal(wrapper);
  }

void
parser_field_attr_set(const cbl_field_t *tgt,
                            cbl_field_attr_t attr,
                            bool on_off )
  {
  if( on_off )
    {
    gg_assign(member(tgt, "attr"),
              gg_bitwise_or(member(tgt, "attr"),
                            build_int_cst_type(SIZE_T, attr)));
    }
  else
    {
    gg_assign(member(tgt, "attr"),
              gg_bitwise_and(member(tgt, "attr"),
                             build_int_cst_type(SIZE_T, ~attr)));
    }
  }

void
parser_symbol_add(struct cbl_field_t *new_var )
  {
  Analyze();
  SHOW_PARSE
    {
    char ach[1024];
    SHOW_PARSE_HEADER

    sprintf(ach, " %2.2u %s<%s> off:" HOST_SIZE_T_PRINT_UNSIGNED " "
                    "msiz:%u cap:%u dig:%u rdig:%d attr:0x" HOST_SIZE_T_PRINT_HEX_PURE " loc:%p",
            new_var->level,
            new_var->name,
            cbl_field_type_str(new_var->type),
            (fmt_size_t)new_var->offset,
            new_var->data.memsize,
            new_var->data.capacity(),
            new_var->data.digits,
            new_var->data.rdigits,
            (fmt_size_t)new_var->attr,
            static_cast<void*>(new_var));
    SHOW_PARSE_TEXT(ach)

    if( is_table(new_var) )
      {
      sprintf(ach, " OCCURS:" HOST_SIZE_T_PRINT_DEC,
              (fmt_size_t)new_var->occurs.ntimes());
      SHOW_PARSE_TEXT(ach)
      }
    const cbl_field_t *parent = parent_of(new_var);
    if( parent )
      {
      sprintf(ach,
              " parent:(" HOST_SIZE_T_PRINT_DEC ")%s",
              (fmt_size_t)new_var->parent,
              parent->name);
      SHOW_PARSE_TEXT(ach)
      }
    else
      {
      // Parent isn't a field
      size_t parent_index = new_var->parent;
      if( parent_index )
        {
        const symbol_elem_t *e = symbol_at(parent_index);
        if( e->type == SymFile )
          {
          sprintf(ach,
                  " parent_file:(" HOST_SIZE_T_PRINT_DEC ")%s",
                  (fmt_size_t)new_var->parent,
                  e->elem.file.name);
          SHOW_PARSE_TEXT(ach)
          if( e->elem.file.attr & external_e )
            {
            sprintf(ach, " (flagged external)");
            SHOW_PARSE_TEXT(ach)
            }
          }
        }
      }

    if( symbol_redefines(new_var) )
      {
      sprintf(ach,
              " redefines:(%p)%s",
              static_cast<void*>(symbol_redefines(new_var)),
              symbol_redefines(new_var)->name);
      SHOW_PARSE_TEXT(ach)
      }

    if(    new_var->type == FldGroup
        || new_var->type == FldAlphanumeric
        || new_var->type == FldNumericEdited
        || new_var->type == FldAlphaEdited
        || new_var->type == FldLiteralA
        )
      {
      if(    new_var->data.initial
          && new_var->data.capacity()
          && !(new_var->attr & intermediate_e) )
        {
        SHOW_PARSE_INDENT
        for(size_t i=0; i<new_var->data.capacity(); i++)
          {
          fprintf(stderr, "%2.2X ", static_cast<unsigned char>(new_var->data.initial[i]));
          }
        }
      }
    if( new_var->data.original() && strlen(new_var->data.original()) )
      {
      SHOW_PARSE_INDENT
      sprintf(ach,
              "\"%s\" (%d)",
              new_var->data.original(),
              static_cast<int>(strlen(new_var->data.original())));
      SHOW_PARSE_TEXT(ach);
      }
    SHOW_PARSE_END
    }

  RETURN_WHEN_HIJACKED;

  if( new_var->level == 1  && new_var->occurs.bounds.upper )
    {
    if( new_var->data.memsize < new_var->data.capacity() * new_var->occurs.bounds.upper )
      {
      cbl_internal_error("LEVEL 01 (%s) OCCURS "
                         "has insufficient data.memsize", new_var->name);
      }
    }

  if( new_var->var_decl_node )
    {
    if( new_var->type == FldConditional )
      {
      gg_assign(new_var->var_decl_node, boolean_false_node);
      }

    goto done;
    }

  if( !(new_var->attr & initialized_e) )
    {
    cbl_field_type_t incoming_type = new_var->type;

    if( new_var->attr & register_e )
      {
      psa_global(new_var);
      goto done;
      }

    if( new_var->type == FldLiteralA )
      {
      new_var->data.picture = "";
      psa_FldLiteralA(new_var);
      goto done;
      }

    size_t length_of_initial_string = 0;
    const char *new_initial = NULL;

    //  Make sure we have a new variable to work with.
    if( !new_var )
      {
      cbl_internal_error("%<parser_symbol_add()%> was called with a NULL %<new_var%>");
      }

    TRACE1
      {
      TRACE1_HEADER
      if( new_var->level )
        {
        gg_fprintf( trace_handle,
                    1,
                    "%2.2d ",
                    build_int_cst_type(INT, new_var->level));
        }
      TRACE1_TEXT(new_var->name)
      TRACE1_TEXT_ABC(" (", cbl_field_type_str(new_var->type) ,")")
      if( new_var->type == FldLiteralN)
        {
        const void *p1 = (new_var->data.initial);
        const long *pldata = static_cast<const long *>(p1);
        long ldata = *pldata;
        gg_fprintf( trace_handle,
                    1, " [%ld]",
                    build_int_cst_type(LONG, ldata));
        }
      TRACE1_END
      }

    if( is_table(new_var) && new_var->data.capacity() == 0)
      {
      cbl_internal_error(
          "%s: %d %s is a table, but it improperly has a capacity of zero",
           __func__,
           new_var->level,
           new_var->name);
      }

    cbl_field_t *ancestor = NULL;
    tree immediate_parent = NULL_TREE;

    if( new_var->parent > 0 )
      {
      symbol_elem_t *parent = symbol_at(new_var->parent);
      gcc_assert(parent);
      if( parent->type == SymField )
        {
        ancestor = cbl_field_of(parent);
        immediate_parent = ancestor->var_decl_node;
        }
      }

    if( ancestor == NULL )
      {
      // This is a last ditch effort for handling SAME AREA.  Although
      // symbol_redefines should work for REDEFINES, LEVEL66, and SAME AREA, I
      // decided to leave the existing code alone and added this in when SAME AREA
      // was added in.
      ancestor = symbol_redefines(new_var);
      if( ancestor )
        {
        immediate_parent = ancestor->var_decl_node;

        // This obscure test was put in to find problems caused by SAME AREA,
        // which at one point would cause a parent to be erroneously seen after
        // the child.
        assert(ancestor->our_index < new_var->our_index);
        }
      }

    if( ancestor == new_var )
      {
      cbl_internal_error("%s: %s is its own ancestor", __func__, new_var->name);
      }

    if( !ancestor && (new_var->level > LEVEL01 && new_var->level <= LEVEL49 ) )
      {
      cbl_internal_error("%s: %d %qs has NULL ancestor", __func__,
                         new_var->level, new_var->name);
      }

    //  new_var's var_decl_node should be NULL at this point
    if( new_var->var_decl_node )
      {
      cbl_internal_error( "%s(%s) improperly has a non-null "
                          "%<var_decl_node%>", __func__, new_var->name);
      }

    switch( new_var->type ) // Trap_here for ordinary variables.
      {
      static int counter=1;
      char ach[2*sizeof(cbl_name_t)];
      case FldConditional:
        // FldConditional corresponds to a C "bool".  But we don't carry
        // a runtime copy of a structure for the variable; instead,
        // var_decl_node becomes a boolean_type_node that is used directly.
        sprintf(ach, "_%sconditional_%d", new_var->name, counter++);
        new_var->var_decl_node = gg_define_variable(BOOL, ach, vs_static);
        goto done;
        break;

      default:
        break;
      }

    if(    new_var->type == FldNumericBinary
        || new_var->type == FldNumericBin5 )
      {
      switch( new_var->data.capacity() )
        {
        case 1:
        case 2:
        case 4:
        case 8:
        case 16:
          break;
        default:
          fprintf(stderr,
                  "%s is type %s and has capacity %u\n",
                  new_var->name,
                  cbl_field_type_str(new_var->type),
                  new_var->data.capacity());
          gcc_unreachable();
          break;
        }
      }

    size_t level_88_string_size = 0;
    char *level_88_string = NULL;
    char *class_string = NULL;
    if( ancestor )
      {
      level_88_string = get_level_88_domain(ancestor->data.capacity(),
                                            new_var,
                                            level_88_string_size);
      if( level_88_string )
        {
        // At this point, the string is in source_code encoding, no matter what
        // the variable's encoding might be.  In the run-time, we will be doing
        // any comparisons of text strings using UTF32 (because that's how we
        // handle somebody specifying a UTF-8 exec-charset.)  Rather than
        // convert this string at run-time, we convert it here:
        size_t converted_length;
        const char *converted = __gg__iconverter(
                                 new_var->codeset.default_encodings.source->type,
                                 HOST_32_ENCODING,
                                 level_88_string,
                                 level_88_string_size,  // Convert the NUL
                                 &converted_length);
        level_88_string_size = converted_length;
        level_88_string = static_cast<char *>(xrealloc(level_88_string,
                                                       level_88_string_size));
        memcpy(level_88_string, converted, level_88_string_size);
        // level_88_string is now a UTF32 string with a terminating four-byte
        // NUL.
        }
      }

    if( !new_var->data.picture )
      {
      // When picture is NULL, we have to keep testing for NULLness at runtime
      // Force it to be a zero-length string here, so that we don't need to
      // worry about it.
      new_var->data.picture = "";
      }

    if( new_var->type == FldNumericEdited && (new_var->attr & scaled_e) )
      {
      char *pic = xstrdup(new_var->data.picture); // duplicate the const char *
      remove_p_from_picture(pic);
      new_var->data.picture = pic;
      }

    if( new_var->type == FldClass && new_var->level != 88 )
      {
      class_string = get_class_condition_string(new_var);
      length_of_initial_string = strlen(class_string)+1;
      new_initial = class_string;
      }
    else if( new_var->type == FldLiteralA )
      {
      length_of_initial_string = new_var->data.capacity();
      }
    else if( new_var->data.original() && new_var->data.original()[0] != '\0' )
      {
      if( new_var->type == FldClass )
        {
        length_of_initial_string = strlen(new_var->data.original())+1;
        }
      else if( new_var->type == FldNumericDisplay )
        {
        length_of_initial_string = strlen(new_var->data.original())+1;
        }
      else
        {
        length_of_initial_string = new_var->data.capacity() + 1;
        }
      }
    else
      {
      // We have something that doesn't have a data.initial pointer
      length_of_initial_string = 0;
      }

    char external_record_base[2*sizeof(cbl_name_t)] = "";

    if( new_var->parent > 0 )
      {
      // new_var has a parent.
      symbol_elem_t *parent = symbol_at(new_var->parent);
      gcc_assert(parent);
      if( parent->type == SymField )
        {
        ancestor = cbl_field_of(parent);
        immediate_parent = ancestor->var_decl_node;
        }
      else if( parent->type == SymFile )
        {
        if( parent->elem.file.attr & external_e )
          {
          // The parent of new_var is a SymFile with the external_e attribute
          // Therefore, we have to establish new_var as an external with a
          // predictable name, which we derive from the source file the parent
          // came from.
          strcpy(external_record_base, parent->elem.file.name);
          }
        }
      }

    tree new_var_decl = psa_new_var_decl(new_var, external_record_base);

    if( new_var->type == FldNumericEdited )
      {
      // Decide if a NumericEdited can hold negative numbers:
      size_t len = strlen( new_var->data.picture);

      new_var->attr &= ~signable_e;
      if( strchr(new_var->data.picture, '+') )
        {
        new_var->attr |= signable_e;
        }
      else if( strchr(new_var->data.picture, '-') )
        {
        new_var->attr |= signable_e;
        }
      else if( len > 2 )
        {
        char ch1 = _toupper(new_var->data.picture[len-2]);
        char ch2 = _toupper(new_var->data.picture[len-1]);
        if(    (ch1 == 'D' && ch2 == 'B')
               || (ch1 == 'C' && ch2 == 'R') )
          {
          new_var->attr |= signable_e;
          }
        }
      }

    /*
     * Burn after reading. (Delete comment after implementing.)
     *
     * As of Tue Apr  4 10:29:35 2023, we support 01 CONSTANT numeric values as follows:
     * 1.  FldNumericBin5
     * 2.  always constant_e, also potentially global_e
     * 3.  compile-time value in cbl_field_data_t::valuer
     * 4.  cbl_field_data_t::capacity is 0 because it requires no working storage
     */

    if(    new_var->data.capacity() == 0
        && !(   new_var->type == FldAlphanumeric
             && new_var->attr & intermediate_e)
        && new_var->level != 88
        && new_var->type  != FldClass
        && new_var->type  != FldLiteralN
        && new_var->type  != FldLiteralA )
      {
      cbl_internal_error(  "%s: %d %s<%s> improperly has a data.capacity of zero",
              __func__,
              new_var->level,
              new_var->name,
              cbl_field_type_str(new_var->type));
      }

    new_var->var_decl_node = new_var_decl;

    if( level_88_string )
      {
      new_var->data.original(level_88_string);
      new_initial = level_88_string;
      length_of_initial_string = level_88_string_size;
      }

    tree data_area = null_pointer_node;

    if( *external_record_base )
      {
      char achDataName[256];
      sprintf(achDataName, "__%s_vardata", external_record_base);
      tree array_type = build_array_type_nelts(UCHAR, new_var->data.capacity());
      new_var->data_decl_node = gg_define_variable(
                          array_type,
                          achDataName,
                          vs_common);
      data_area = gg_pointer_to_array(new_var->data_decl_node);
      goto actual_allocate;
      }

    if( ancestor && new_var->level != 00 )
      {
      // This variable has an ancestor, so we share its already-allocated data
      // area
      new_var->data_decl_node = ancestor->data_decl_node;
      }
    else
      {
      // We have no ancestor, so data_decl_node must be allocated.  Note that
      // LEVEL00 variables might have ancestors (INDEXED BY variables, for
      // example), but they need data allocated.

      if( new_var->type == FldLiteralN )
        {
        // A numeric literal gets special handling:
        psa_FldLiteralN(new_var);
        data_area = gg_get_address_of(new_var->data_decl_node);
        }
      else
        {
        // Create a static array of UCHAR, and make that the data_decl_node
        // size_t bytes_to_allocate = new_var->data.memsize ?
                                // new_var->data.memsize : new_var->data.capacity();
        size_t bytes_to_allocate = std::max(new_var->data.memsize,
                                            new_var->data.capacity());

        // A FldClass actually doesn't need any bytes, because the only important
        // thing about it is the .initial field.  We will allocate a single byte,
        // just to keep run-time pointers from being NULL
        if(    (new_var->type == FldClass    && bytes_to_allocate == 0)  )
          {
          bytes_to_allocate = 1;
          }

        if( !bytes_to_allocate && !(new_var->attr & intermediate_e) )
          {
          cbl_internal_error( "%<bytes_to_allocate%> is zero for %s (symbol number "
                              HOST_SIZE_T_PRINT_DEC ")",
                              new_var->name,
                              (fmt_size_t)new_var->our_index);
          }

        if( new_var->type == FldIndex && new_var->level == 0 )
          {
          // Do nothing, because the OCCURS INDEXED BY variable needs data
          // allocated.  This leaves bytes_to_allcate at its value.
          }
        else
          {
          if(    new_var->attr & based_e
              || new_var->attr & linkage_e
              || new_var->attr & local_e )
            {
            // BASED   variables get their data through ALLOCATE or SET
            // LINKAGE variables get their data from the caller
            // LOCAL   variables get their data dynamically.
            bytes_to_allocate = 0;
            }
          }

        if(   (new_var->attr & intermediate_e)
           && new_var->type == FldAlphanumeric )
          {
          // We don't allocate here for intermediates.  We instead use
          // malloc() in the library when a run-time value is assigned to this
          // variable
          data_area = null_pointer_node;
          }
        else
          {
          if( bytes_to_allocate )
            {
            // We need a unique name for the allocated data for this COBOL variable:
            char achDataName[256];
            if( new_var->attr & external_e )
              {
              sprintf(achDataName, "%s", new_var->name);
              }
            else if( new_var->name[0] == '_' )
              {
              // Avoid doubling up on leading underscore
              sprintf(achDataName,
                      "%s_data_" HOST_SIZE_T_PRINT_UNSIGNED,
                      new_var->name,
                      (fmt_size_t)sv_data_name_counter++);
              }
            else
              {
              sprintf(achDataName,
                      "_%s_data_" HOST_SIZE_T_PRINT_UNSIGNED,
                      new_var->name,
                      (fmt_size_t)sv_data_name_counter++);
              }

            if( new_var->attr & external_e )
              {
              tree array_type = build_array_type_nelts(UCHAR, bytes_to_allocate);
              new_var->data_decl_node = gg_define_variable(
                                  array_type,
                                  achDataName,
                                  vs_common);
              data_area = gg_pointer_to_array(new_var->data_decl_node);
              }
            else
              {
              gg_variable_scope_t vs_scope = (new_var->attr & intermediate_e)
                                           ? vs_stack
                                           : vs_static ;
              tree data_decl_type = data_decl_type_for(new_var);
              new_var->data_decl_node = gg_define_variable( data_decl_type,
                                                            achDataName,
                                                            vs_scope);
              if( TREE_CODE(data_decl_type) == ARRAY_TYPE )
                {
                data_area = gg_pointer_to_array(new_var->data_decl_node);
                }
              else
                {
                data_area = gg_get_address_of(new_var->data_decl_node);
                }
              }
            }
          }
        }
      }

    // At this point, new_initial might have been set by
    // get_class_condition_string.  If not, we set it another way:
    if( !level_88_string && !class_string)
      {
      new_initial = const_cast<char *>(new_var->data.initial);
      length_of_initial_string = new_var->data.capacity();
      }

    actual_allocate:
    actually_create_the_static_field( new_var,
                                      data_area,
                                      length_of_initial_string,
                                      new_initial,
                                      immediate_parent,
                                      new_var_decl);
    free(level_88_string);
    free(class_string);

    if(    !(new_var->attr & ( linkage_e | based_e ))
        && !(new_var->type == FldLiteralN) )
      {
      static const bool explicitly = false;
      static const bool just_once = true;
      initialize_variable_internal( new_var,
                                    explicitly,
                                    just_once);
      }

    if( new_var->type != incoming_type )
      {
      fprintf(stderr, "Type mismatch in parser_symbol_add()\n");
      gcc_unreachable();
      }
    new_var->attr |= initialized_e;
    }
  else
    {
    fprintf(stderr, "parser_symbol_add() skipping %s", new_var->name);
    }
  done:
  return;
  }

