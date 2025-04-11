/*
 * Copyright (c) 2021-2025 Symas Corporation
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
#ifndef SHOW_PARSE_H_
#define SHOW_PARSE_H_

// These macros provide information about what the compiler is doing,
// and about what the compiled code is doing.

// SHOW_PARSE gives information when parser_xxx functions are entered, and
// then attempts to give as much information as it can at compile time about
// variables and their characteristics, the contents of literals, and such.  It
// doesn't affect the executable at all.

// TRACE1 lays down code for run-time tracing.

// SHOW_PARSE must be followed by a bracketed set of instructions, no semicolon

extern bool bSHOW_PARSE;
extern bool show_parse_sol;
extern int  show_parse_indent;

extern char const *bTRACE1;
extern tree trace_handle;
extern tree trace_indent;
extern bool cursor_at_sol;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

#define RETURN_IF_PARSE_ONLY                    \
  do { if(  mode_syntax_only() ) return; } while(0)

#define SHOW_PARSE1                      if(bSHOW_PARSE)
#define SHOW_PARSE RETURN_IF_PARSE_ONLY; if(bSHOW_PARSE)

// _HEADER and _END are generally the first and last things inside the
// SHOW_PARSE statement.  They don't have to be; SHOW_PARSE can be used
// anywhere
#define SHOW_PARSE_HEADER do \
  { \
  if(!show_parse_sol){fprintf(stderr, "\n");} \
  show_parse_indent=fprintf(stderr, \
                            "( %d ) %s():" , \
                            (CURRENT_LINE_NUMBER), __func__); \
  show_parse_sol=false; \
  }while(0);
#define SHOW_PARSE_END do{fprintf(stderr, "\n");show_parse_sol=true;}while(0);

// This does one simple text string
#define SHOW_PARSE_TEXT(a) do \
  { \
  fprintf(stderr, "%s", a); \
  show_parse_sol=false; \
  }while(0);

#define SHOW_PARSE_INDENT do{ \
          if(!show_parse_sol){fprintf(stderr, "\n");} \
          for(int i=0; i<show_parse_indent-1; i++) \
            {fprintf(stderr, " ");} \
          fprintf(stderr, ": "); \
          show_parse_sol=false; \
          }while(0);

// This does three simple text strings.
#define SHOW_PARSE_TEXT_AB(pre, a, post) do \
  { \
  SHOW_PARSE_TEXT(pre);SHOW_PARSE_TEXT(a);SHOW_PARSE_TEXT(post) \
  }while(0);

//
#define SHOW_PARSE_FIELD(pre, b) \
        do \
            { \
            fprintf(stderr, "%s", pre); \
            if( !(b) ) \
                { \
                fprintf(stderr, "parameter " #b " is NULL"); \
                } \
            else \
                { \
                fprintf(stderr, "%s", (b)->name); \
                if( (b)->type == FldLiteralA || (b)->type == FldLiteralN ) \
                    { \
                    fprintf(stderr, " \"%s\"", (b)->data.initial); \
                    } \
                else \
                    { \
                    fprintf(stderr, "<%s>", cbl_field_type_str((b)->type)); \
                    } \
                } \
            show_parse_sol = false; \
            } while(0);

#define SHOW_PARSE_REF(pre, b) \
        do \
            { \
            fprintf(stderr, "%s", pre); \
            if( !(b).field ) \
                { \
                fprintf(stderr, "parameter " #b".field is NULL"); \
                } \
            else \
                { \
                fprintf(stderr, "%s", (b).field->name); \
                if( (b).field->type == FldLiteralA || (b).field->type == FldLiteralN ) \
                    { \
                    fprintf(stderr, " \"%s\"", (b).field->data.initial); \
                    } \
                else \
                    { \
                    fprintf(stderr, "<%s>", cbl_field_type_str((b).field->type)); \
                    } \
                } \
            if( (b).nsubscript) \
                { \
                fprintf(stderr,"("); \
                for(size_t jjj=0; jjj<(b).nsubscript; jjj++) \
                    { \
                    if(jjj) \
                      { \
                      SHOW_PARSE_FIELD(" ", (b).subscripts[jjj].field) \
                      } \
                    else \
                      { \
                      SHOW_PARSE_FIELD("", (b).subscripts[jjj].field) \
                      } \
                    } \
                fprintf(stderr,")"); \
                } \
            show_parse_sol = false; \
            } while(0);

#define SHOW_PARSE_LABEL(a, b) \
        do \
            { \
            fprintf(stderr, "%s", a); \
            if( !b ) \
                { \
                fprintf(stderr, "label " #b " is NULL"); \
                } \
            else \
                { \
		fprintf(stderr, " %p:%s (%s)", (void*)b, b->name, b->type_str()); \
                } \
            show_parse_sol = false; \
            } while(0);

#define TRACE1 if(bTRACE1)
#define TRACE1_HEADER do \
  { \
  if(!cursor_at_sol){gg_fprintf(trace_handle , 0, "\n");} \
  gg_assign(trace_indent, \
            gg_fprintf( trace_handle , \
                        2, \
                        ">>>>>>( %d )(%s) ", \
                        build_int_cst_type(INT, CURRENT_LINE_NUMBER), \
                        gg_string_literal(__func__))); \
  }while(0);

#define TRACE1_INDENT do{ \
  if(!cursor_at_sol){gg_fprintf(trace_handle , 0, "\n");} \
  tree counter = gg_define_int(); \
  gg_assign(counter, integer_zero_node); \
  WHILE(counter, lt_op, trace_indent) \
    gg_fprintf(trace_handle , 0, " "); \
    gg_increment(counter); \
    WEND \
  }while(0);

#define TRACE1_END do{gg_fprintf(trace_handle, 0, "\n");cursor_at_sol=true;}while(0);

#define TRACE1_TEXT(a) do{cursor_at_sol=false;gg_fprintf(trace_handle, 1, "%s", gg_string_literal(a));}while(0);
#define TRACE1_TEXT_ABC(a,b,c) do{TRACE1_TEXT(a);TRACE1_TEXT(b);TRACE1_TEXT(c)}while(0);

#define TRACE1_FIELD_VALUE(a, field, b) \
        do \
            { \
            cursor_at_sol=false; \
            if ( field->type == FldConditional ) \
              {                                       \
              gg_fprintf(trace_handle, 1, "%s \"", gg_string_literal(a)); \
              parser_display_internal_field(trace_handle, field, false); \
              gg_fprintf(trace_handle, 1, "\" %s", gg_string_literal(b)); \
              }                                       \
            else \
              { \
              IF( member(field->var_decl_node, "data"), eq_op, gg_cast(UCHAR_P, null_pointer_node) )  \
                {   \
                gg_fprintf(trace_handle, 1, "%s ", gg_string_literal(a)); \
                gg_fprintf(trace_handle, 0, "NULL"); \
                gg_fprintf(trace_handle, 1, " %s", gg_string_literal(b)); \
                }   \
              ELSE  \
                {   \
                if(   field->type == FldGroup \
                   || field->type == FldAlphanumeric  \
                   || field->type == FldAlphaEdited  \
                   || field->type == FldLiteralA ) \
                  { \
                  gg_fprintf(trace_handle, 1, "%s \"", gg_string_literal(a)); \
                  parser_display_internal_field(trace_handle, field, false); \
                  gg_fprintf(trace_handle, 1, "\" %s", gg_string_literal(b)); \
                  } \
                else \
                  { \
                  gg_fprintf(trace_handle, 1, "%s [", gg_string_literal(a)); \
                  parser_display_internal_field(trace_handle, field, false); \
                  gg_fprintf(trace_handle, 1, "] %s", gg_string_literal(b)); \
                  } \
                } \
              ENDIF \
              } \
            }while(0);

#define TRACE1_REFER_VALUE(a, refer, b) \
        do \
            { \
            if( refer.field ) \
              { \
              cursor_at_sol=false; \
              IF( member(refer.field->var_decl_node, "data"), eq_op, gg_cast(UCHAR_P, null_pointer_node) )  \
                {   \
                gg_fprintf(trace_handle, 1, "%s ", gg_string_literal(a)); \
                gg_fprintf(trace_handle, 0, "NULL"); \
                gg_fprintf(trace_handle, 1, " %s", gg_string_literal(b)); \
                }   \
              ELSE  \
                {   \
                if(   refer.field->type == FldGroup \
                   || refer.field->type == FldAlphanumeric  \
                   || refer.field->type == FldAlphaEdited  \
                   || refer.field->type == FldLiteralA ) \
                  { \
                  gg_fprintf(trace_handle, 1, "%s \"", gg_string_literal(a)); \
                  parser_display_internal(trace_handle, refer, false); \
                  gg_fprintf(trace_handle, 1, "\" %s", gg_string_literal(b)); \
                  } \
                else \
                  { \
                  gg_fprintf(trace_handle, 1, "%s [", gg_string_literal(a)); \
                  parser_display_internal(trace_handle, refer, false); \
                  gg_fprintf(trace_handle, 1, "] %s", gg_string_literal(b)); \
                  } \
                } \
              ENDIF \
              } \
            else \
              { \
              gg_fprintf(trace_handle, 0, "refer.field is NULL"); \
              } \
            }while(0);

#define TRACE1_FIELD_INFO(pre, b) \
  do{ \
    cursor_at_sol=false; \
    gg_fprintf(trace_handle, 1, "%s", gg_string_literal(pre)); \
    if( !b ) \
      { \
      gg_fprintf(trace_handle, 0, "field " #b " is NULL"); \
      } \
    else \
      { \
      gg_fprintf(trace_handle, 1, "%s", gg_string_literal(b->name)); \
      gg_fprintf(trace_handle, 1, " (%s", gg_string_literal(cbl_field_type_str((b)->type))); \
      if( b->type != FldLiteralN && b->type != FldConditional ) \
        { \
        cbl_field_t* B(b); \
        if( !b->var_decl_node ) \
          { \
          gg_fprintf(trace_handle, 0, #b "->var_decl_node is NULL", NULL_TREE); \
          } \
        else \
          { \
          gg_fprintf(trace_handle, 1, " attr 0x%lx",  member(B, "attr"    )); \
          gg_fprintf(trace_handle, 1, " c:o:d:r %ld", member(B, "capacity")); \
          gg_fprintf(trace_handle, 1, ":%ld",         member(B, "offset"  )); \
          gg_fprintf(trace_handle, 1, ":%d",          gg_cast(INT, (member(B, "digits"  )))); \
          gg_fprintf(trace_handle, 1, ":%d",         gg_cast(INT, (member(B, "rdigits" )))); \
          } \
        } \
      else if( b->type == FldLiteralN ) \
        { \
        gg_fprintf(trace_handle, 1, " attr 0x%lx",  build_int_cst_type(SIZE_T, b->attr)); \
        gg_fprintf(trace_handle, 1, " c:o:d:r %ld", build_int_cst_type(SIZE_T, b->data.capacity)); \
        gg_fprintf(trace_handle, 1, ":%ld",         build_int_cst_type(SIZE_T, b->offset)); \
        gg_fprintf(trace_handle, 1, ":%d",          build_int_cst_type(INT,    b->data.digits)); \
        gg_fprintf(trace_handle, 1, ":%d",         build_int_cst_type(INT,    b->data.rdigits)); \
        } \
      gg_fprintf(trace_handle, 0, ")"); \
      } \
    }while(0);

#define TRACE1_REFER_INFO(pre, b) \
  do{ \
    cursor_at_sol=false; \
    gg_fprintf(trace_handle, 1, "%s", gg_string_literal(pre)); \
    if( !(b).field ) \
      { \
      gg_fprintf(trace_handle, 0, #b ".field is NULL"); \
      } \
    else \
      { \
      gg_fprintf(trace_handle, 1, "%s", gg_string_literal( (b).field->name ? (b).field->name:"")); \
      if( b.nsubscript ) \
        { \
        gg_fprintf(trace_handle, 0, "("); \
        for(unsigned int i=0; i<b.nsubscript; i++) \
          { \
          gg_fprintf(trace_handle, 1, "%s", gg_string_literal(    b.subscripts[i].field->name ? b.subscripts[i].field->name : ""  )); \
          if( i<b.nsubscript-1 ) \
            { \
            gg_fprintf(trace_handle, 0, " "); \
            } \
          } \
        if( b.refmod.from || b.refmod.len ) \
          { \
          gg_fprintf(trace_handle, 0, "("); \
          if( b.refmod.from ) \
            { \
              gg_fprintf(trace_handle, 1, "%s", gg_string_literal(   b.refmod.from->name() ? b.refmod.from->name() : "" )); \
            } \
          gg_fprintf(trace_handle, 0, ":"); \
          if( b.refmod.len ) \
            { \
              gg_fprintf(trace_handle, 1, "%s", gg_string_literal(    b.refmod.len->name()   ? b.refmod.len->name() : "" )); \
            } \
          gg_fprintf(trace_handle, 0, "("); \
          } \
        gg_fprintf(trace_handle, 0, ")"); \
        } \
      gg_fprintf(trace_handle, 1, " (%s", gg_string_literal(cbl_field_type_str((b).field->type))); \
      if( (b).field->type != FldLiteralN && (b).field->type != FldConditional ) \
        { \
        if( !(b).field->var_decl_node ) \
          { \
          gg_fprintf(trace_handle, 0, #b ".field->var_decl_node is NULL", NULL_TREE); \
          } \
        else \
          { \
          gg_fprintf(trace_handle, 1, " attr 0x%lx",  member(b.field, "attr"    )); \
          gg_fprintf(trace_handle, 1, " c:o:d:r %ld", member(b.field, "capacity")); \
          gg_fprintf(trace_handle, 1, ":%ld",         member(b.field, "offset"  )); \
          gg_fprintf(trace_handle, 1, ":%d",          gg_cast(INT, (member(b.field, "digits"  )))); \
          gg_fprintf(trace_handle, 1, ":%d)",         gg_cast(INT, (member(b.field, "rdigits" )))); \
          } \
        } \
      else if( (b).field->type == FldLiteralN ) \
        { \
        gg_fprintf(trace_handle, 1, " attr 0x%lx",  build_int_cst_type(SIZE_T, (b).field->attr)); \
        gg_fprintf(trace_handle, 1, " c:o:d:r %ld", build_int_cst_type(SIZE_T, (b).field->data.capacity)); \
        gg_fprintf(trace_handle, 1, ":%ld",         build_int_cst_type(SIZE_T, (b).field->offset)); \
        gg_fprintf(trace_handle, 1, ":%d",          build_int_cst_type(INT,    (b).field->data.digits)); \
        gg_fprintf(trace_handle, 1, ":%d)",         build_int_cst_type(INT,    (b).field->data.rdigits)); \
        } \
      } \
    }while(0);

#define TRACE1_FIELD(a, b, c) \
  do{ \
  TRACE1_FIELD_INFO(a, b)      \
  TRACE1_FIELD_VALUE("", b, c) \
  }while(0);

#define TRACE1_REFER(a, b, c) \
  do{ \
  TRACE1_REFER_INFO(a, b) \
  TRACE1_REFER_VALUE("", b, c) \
  }while(0);

#define TRACE1_LABEL(a, b, c) \
  do{ \
    cursor_at_sol=false; \
    gg_fprintf(trace_handle, 1, "%s", gg_string_literal(a)); \
    if( !b ) \
        { \
        gg_fprintf(trace_handle, 0, "label " #b " is NULL"); \
        } \
    else \
        { \
        gg_fprintf(trace_handle, 2, \
                "%s (%s)", \
                gg_string_literal(b->name), \
                gg_string_literal(b->type_str()), \
                NULL_TREE); \
        } \
    gg_fprintf(trace_handle, 1, "%s", gg_string_literal(c)); \
    } while(0);

// Use CHECK_FIELD when a should be non-null, and a->var_decl_node also should
// by non-null:
#define CHECK_FIELD(a)          \
        do{                     \
        if(!a)                  \
            {                   \
            yywarn("%s(): parameter " #a " is NULL", __func__); \
            gcc_unreachable();  \
            }                   \
        if( !a->var_decl_node && a->type != FldConditional && a->type != FldLiteralA)  \
            {                   \
            yywarn("%s() parameter " #a " is variable %s<%s> with NULL var_decl_node", \
                __func__,       \
                a->name,        \
                cbl_field_type_str(a->type) ); \
            gcc_unreachable();  \
            }                   \
        }while(0);

#define CHECK_LABEL(a)            \
        do{                     \
        if(!a)                  \
            {                   \
            yywarn("%s(): parameter " #a " is NULL", __func__); \
            gcc_unreachable();  \
            }                   \
        }while(0);

#ifdef INCORPORATE_ANALYZER
// The analyzer requires a C++17 compiler because of the inline static variable
class ANALYZE
  {
  private:
    const char *func;
    int level;
    inline static int analyze_level=1;
  public:
    ANALYZE(const char *func_) : func(func_)
      {
      level = 0;
      if( getenv("Analyze") )
        {
        level = analyze_level++;
        char ach[128];
        snprintf(ach, sizeof(ach), "# %s analyze_enter %d", func, level);
        if( !mode_syntax_only() )
          {
          gg_insert_into_assembler(ach);
          }
        }
      }
    ~ANALYZE()
      {
      ExitMessage();
      }
    void ExitMessage()
      {
      if( getenv("Analyze") )
        {
        char ach[128];
        snprintf(ach, sizeof(ach), "# %s analyze_exit %d", func, level);
        if( !mode_syntax_only() )
          {
          gg_insert_into_assembler(ach);
          }
        }
      }
    void Message(const char *msg)
      {
      if( getenv("Analyze") )
        {
        char ach[128];
        snprintf(ach, sizeof(ach), "# %s %s %d", func, msg, level);
        if( !mode_syntax_only() )
          {
          gg_insert_into_assembler(ach);
          }
        }
      }
  };
#else
class ANALYZE
  {
  public:
    ANALYZE(const char *)
      {
      }
    ~ANALYZE()
      {
      ExitMessage();
      }
    void ExitMessage()
      {
      }
    void Message(const char *)
      {
      }
  };
#endif

#define Analyze() ANALYZE Analyzer(__func__);

#pragma GCC diagnostic pop

#endif
