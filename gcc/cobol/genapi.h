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
#ifndef _GENAPI_H_
#define _GENAPI_H_

#define DISPLAY_ADVANCE true
#define DISPLAY_NO_ADVANCE false

typedef enum
  {
  refer_dest,
  refer_source,
  } refer_type_t;

void parser_display_internal( tree file_descriptor,
                              cbl_refer_t refer,
                              bool advance=DISPLAY_NO_ADVANCE);

void parser_first_statement( int lineno );

void parser_enter_file(const char *filename);
void parser_leave_file();
void parser_division( cbl_division_t division,
                      cbl_field_t *ret, size_t narg, cbl_ffi_arg_t args[] );
void parser_enter_program(const char *funcname, bool is_function, int *retval);
void parser_leave_program();

void parser_accept( cbl_refer_t refer, special_name_t special_e);
void parser_accept_exception( cbl_label_t *name );
void parser_accept_exception_end( cbl_label_t *name );

void parser_accept_envar( cbl_refer_t refer, cbl_refer_t envar,
                          cbl_label_t *error, cbl_label_t *not_error );
void parser_set_envar( cbl_refer_t envar, cbl_refer_t refer );

void parser_accept_command_line( cbl_refer_t tgt, cbl_refer_t src,
                          cbl_label_t *error, cbl_label_t *not_error );
void parser_accept_command_line_count( cbl_refer_t tgt );

void parser_accept_date_yymmdd( cbl_field_t *tgt );
void parser_accept_date_yyyymmdd( cbl_field_t *tgt );
void parser_accept_date_yyddd( cbl_field_t *tgt );
void parser_accept_date_yyyyddd( cbl_field_t *tgt );
void parser_accept_date_dow( cbl_field_t *tgt );
void parser_accept_date_hhmmssff( cbl_field_t *tgt );

void
parser_alphabet( cbl_alphabet_t& alphabet );
void
parser_alphabet_use( cbl_alphabet_t& alphabet );

void
parser_allocate( cbl_refer_t size_or_based, cbl_refer_t returning, bool initialized );
void
parser_free( size_t n, cbl_refer_t refers[] );

void
parser_add( size_t nC, cbl_num_result_t *C,
            size_t nA, cbl_refer_t *A,
            cbl_arith_format_t format,
            cbl_label_t *error,
            cbl_label_t *not_error,
            void *compute_error = NULL);  // This has to be cast to a tree pointer to int

void parser_arith_error( cbl_label_t *name );
void parser_arith_error_end( cbl_label_t *name );

void
parser_subtract(size_t nC, cbl_num_result_t *C,
                size_t nA, cbl_refer_t *A,
                size_t nB, cbl_refer_t *B,
                cbl_arith_format_t format,
                cbl_label_t *error,
                cbl_label_t *not_error,
                void *compute_error = NULL);  // This has to be cast to a tree pointer to int

void
parser_multiply(size_t nC, cbl_num_result_t *C,
                size_t nA, cbl_refer_t *A,
                size_t nB, cbl_refer_t *B,
                cbl_label_t *error,
                cbl_label_t *not_error,
                void *compute_error = NULL);  // This has to be cast to a tree pointer to int

void
parser_divide(size_t nC, cbl_num_result_t *C,
              size_t nA, cbl_refer_t *A,
              size_t nB, cbl_refer_t *B,
              cbl_refer_t remainder,
              cbl_label_t *error,
              cbl_label_t *not_error,
              void *compute_error = NULL);  // This has to be cast to a tree pointer to int

void
parser_add( struct cbl_refer_t tgt,
            struct cbl_refer_t a, struct cbl_refer_t b,
            enum cbl_round_t = truncation_e );

void
parser_subtract( struct cbl_refer_t tgt,
                 struct cbl_refer_t a, struct cbl_refer_t b,
                 enum cbl_round_t = truncation_e );

void
parser_multiply( struct cbl_refer_t tgt,
                 struct cbl_refer_t a, struct cbl_refer_t b,
                 enum cbl_round_t = truncation_e );

void
parser_divide( struct cbl_refer_t quotient,
               struct cbl_refer_t divisor,
               struct cbl_refer_t dividend,
               enum cbl_round_t = truncation_e,
               struct cbl_refer_t remainder = cbl_refer_t());

// void
// parser_exponentiation(  cbl_refer_t cref,
//                         cbl_refer_t aref,
//                         cbl_refer_t bref,
//                         cbl_round_t rounded = truncation_e );

void
parser_relop( struct cbl_field_t *tgt,
              struct cbl_refer_t a, enum relop_t, struct cbl_refer_t b );

void
parser_relop_long(struct cbl_field_t *tgt,
                  long a, enum relop_t, struct cbl_refer_t b );

void
parser_logop( struct cbl_field_t *tgt,
              struct cbl_field_t *a,  enum logop_t, struct cbl_field_t *b );

void
parser_setop( struct cbl_field_t *tgt,
              struct cbl_field_t *a,  enum setop_t, struct cbl_field_t *b );

void
parser_bitop( struct cbl_field_t *tgt,
              struct cbl_field_t *a,  enum bitop_t, size_t B );

void
parser_bitwise_op(struct cbl_field_t *tgt,
                  struct cbl_field_t *a,
                  enum bitop_t op,
                  size_t bitmask );

void
parser_classify( struct cbl_field_t *tgt,
                 struct cbl_refer_t  srca,  enum classify_t type );

void
parser_op( struct cbl_refer_t cref,
           struct cbl_refer_t aref, int op, struct cbl_refer_t bref,
           struct cbl_label_t *op_error);

cbl_field_t
determine_intermediate_type( const cbl_refer_t& aref,
                             int op,
                             const cbl_refer_t& bref );

void
parser_if( struct cbl_field_t *yn ); // value is 1 or 0
void
parser_else(void);
void
parser_fi(void);


void
parser_enter_paragraph( struct cbl_label_t *label );
void
parser_leave_paragraph( cbl_label_t *label );

void
parser_enter_section( struct cbl_label_t *label );
void
parser_leave_section( struct cbl_label_t *label );

void
parser_perform( struct cbl_label_t *label, bool suppress_nexting=false );

void
parser_perform_times( struct cbl_label_t *label, cbl_refer_t count );

void
parser_perform_start( struct cbl_perform_tgt_t *tgt );

void
parser_perform_conditional( struct cbl_perform_tgt_t *tgt );

void
parser_perform_conditional_end( struct cbl_perform_tgt_t *tgt );

/*
 * To perform once (not a loop) N is NULL because the user didn't provide a count.
 * tgt->to is NULL if the PERFORM statement has no THRU phrase.
 * For an in-line loop body, tgt->from.type == LblLoop, and tgt->to is NULL.
 */
void
parser_perform( struct cbl_perform_tgt_t *tgt, struct cbl_refer_t N );

/*
 * A simple UNTIL loop uses 1 varys element.  For VARY loops, the
 * VARY/AFTER phrases appear in varys in the same order as in the
 * COBOL text.
 */

// Either parser_perform_until() or parser_perform_inline_times() must appear
// after a parser_perform_start()
void
parser_perform_until(   struct cbl_perform_tgt_t *tgt,
                        bool test_before,
                        size_t nvary,
                        struct cbl_perform_vary_t *varys );

void
parser_perform_inline_times(struct cbl_perform_tgt_t *tgt,
                            struct cbl_refer_t count );

void
parser_see_stop_run( struct cbl_refer_t exit_status, const char name[] );

void
parser_program_hierarchy( const struct cbl_prog_hier_t& hier );
void
parser_end_program(const char *name=NULL);

void parser_sleep(cbl_refer_t seconds);

void parser_exit( cbl_refer_t refer, ec_type_t = ec_none_e );
void parser_exit_section(void);
void parser_exit_paragraph(void);
void parser_exit_perform( struct cbl_perform_tgt_t *tgt, bool cycle );
void parser_exit_program(void); // exits back to COBOL only, else continue

void
parser_display( const struct cbl_special_name_t *upon,
                struct cbl_refer_t args[], size_t n,
                bool advance = DISPLAY_ADVANCE );

void parser_display_field(cbl_field_t *fld);

void parser_display_literal(const char *literal,
                            bool advance = DISPLAY_ADVANCE);

void
parser_assign( size_t nC, cbl_num_result_t *C,
               struct cbl_refer_t from,
               cbl_label_t *on_error,
               cbl_label_t *not_error,
               cbl_label_t *compute_error );

void parser_move(struct cbl_refer_t to,
                 struct cbl_refer_t from,
                 cbl_round_t rounded=truncation_e,
     bool skip_fill_from = false);

void parser_move( size_t ntgt, cbl_refer_t *tgts,
                  cbl_refer_t src, cbl_round_t rounded=truncation_e );

void parser_initialize_table( size_t ntgt, cbl_refer_t src,
                              size_t nspan, const cbl_bytespan_t spans[],
                              size_t table, // symbol table index
                              size_t ntbl, const cbl_subtable_t tbls[] );

void parser_set_pointers( size_t ntgt, cbl_refer_t *tgts, cbl_refer_t src );

void
parser_symbol_add(struct cbl_field_t *field);

void
parser_initialize(struct cbl_refer_t refer, bool like_parser_symbol_add=false);

void
parser_initialize_programs(size_t nprog, struct cbl_refer_t *progs);

void
parser_label_label( struct cbl_label_t *label );

void
parser_label_goto( struct cbl_label_t *label );

void
parser_goto( cbl_refer_t value, size_t narg, cbl_label_t * const labels[] );

void
parser_alter( cbl_perform_tgt_t *tgt );

void
parser_set_conditional88( struct cbl_refer_t tgt, bool which_way );
void
parser_set_numeric(struct cbl_field_t *tgt, ssize_t value);

void
parser_field_attr_set( cbl_field_t *tgt, cbl_field_attr_t attr, bool on_off = true );

void
parser_file_add(struct cbl_file_t *file);

void
parser_file_open( struct cbl_file_t *file, int mode_char );
void
parser_file_open( size_t n, struct cbl_file_t *files[], int mode_char );

void
parser_file_close( struct cbl_file_t *file, file_close_how_t how = file_close_no_how_e);

void
parser_file_read( struct cbl_file_t *file,
                  struct cbl_refer_t buffer,
                  int where );

void
parser_file_start( struct cbl_file_t *file, relop_t op, int flk,
                   cbl_refer_t = cbl_refer_t() );

/*
 * Write *field* to *file*.  *after* is a bool where false
 * means BEFORE. *nlines* is the number of lines, frequently
 * FldLiteralN.  To indicate PAGE, nlines is the literal "PAGE", with
 * quoted_e off.
 *
 * According to the 2014 standard, the lack of an ADVANCING clause implies
 * AFTER ADVANCING 1 LINE.  *nlines* is be zero to write a line without
 * prepending or appending newlines.  See section 14.9.47.1 paragraph 22)
 *
 * At present, we don't have enough information to implement PAGE
 * correctly, because we don't know the page size (in lines) of the
 * output device. Rather than doing nothing, we issue a 0x0C form feed
 * character.
 */
void
parser_file_write(  cbl_file_t *file,
                    cbl_field_t *source,
                    bool after,
                    cbl_refer_t& nlines,
                    bool sequentially);

void
parser_file_rewrite( cbl_file_t *file, cbl_field_t *field,
                     bool sequentially );

void
parser_file_delete( cbl_file_t *file, bool sequentially );

#if condition_lists
struct cbl_conditional_t {
  cbl_field_t *tgt;
  cbl_refer_t& lhs;
  unsigned int op;
  cbl_refer_t& rhs;
};
#endif

void
parser_lsearch_start(   cbl_label_t *name,
                        cbl_field_t *table,
                        cbl_field_t *index,
                        cbl_field_t *varying );

void parser_lsearch_conditional(cbl_label_t * name);
void parser_bsearch_conditional(cbl_label_t * name);

void parser_lsearch_when( cbl_label_t *name, cbl_field_t *conditional );
void
parser_bsearch_when(cbl_label_t *name,
                    cbl_refer_t key,
                    cbl_refer_t sarg,
                    bool ascending);

void parser_lsearch_end( cbl_label_t *name );
void parser_bsearch_end( cbl_label_t *name );

void
parser_bsearch_start( cbl_label_t *name, cbl_field_t *tgt );

void
parser_sort(cbl_refer_t table,
            bool duplicates,
            cbl_alphabet_t *alphabet,
            size_t nkey,
            cbl_key_t *keys );
void
parser_file_sort(   cbl_file_t *file,
                    bool duplicates,
                    cbl_alphabet_t *alphabet,
                    size_t nkey,
                    cbl_key_t *keys,
                    size_t ninput,
                    cbl_file_t **inputs,
                    size_t noutput,
                    cbl_file_t **outputs,
                    cbl_perform_tgt_t *in_proc,
                    cbl_perform_tgt_t *out_proc );
void
parser_file_merge(  cbl_file_t *file,
                    cbl_alphabet_t *alphabet,
                    size_t nkey,
                    cbl_key_t *keys,
                    size_t ninput,
                    cbl_file_t **inputs,
                    size_t noutput,
                    cbl_file_t **outputs,
                    cbl_perform_tgt_t *out_proc );

void
parser_release( cbl_field_t *record_area );

void
parser_exception_file( cbl_field_t *tgt, cbl_file_t* file = NULL );

void
parser_module_name( cbl_field_t *tgt, module_type_t type );

void
parser_intrinsic_numval_c( cbl_field_t *f,
                           cbl_refer_t& input,
                           bool locale,
                           cbl_refer_t& currency,
                           bool anycases,
                           bool test_numval_c = false);

void
parser_intrinsic_subst( cbl_field_t *f,
                        cbl_refer_t& ref1,
                        size_t argc,
                        cbl_substitute_t * argv );

void
parser_intrinsic_callv( cbl_field_t *f,
                        const char name[],
                        size_t argc,
                        cbl_refer_t * argv );

void
parser_intrinsic_call_0( cbl_field_t *tgt,
                       const char name[] );
void
parser_intrinsic_call_1( cbl_field_t *tgt,
                       const char name[],
                       cbl_refer_t& ref1 );
void
parser_intrinsic_call_2( cbl_field_t *tgt,
                       const char name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2 );
void
parser_intrinsic_call_3( cbl_field_t *tgt,
                       const char name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2,
                       cbl_refer_t& ref3 );
void
parser_intrinsic_call_4( cbl_field_t *tgt,
                       const char name[],
                       cbl_refer_t& ref1,
                       cbl_refer_t& ref2,
                       cbl_refer_t& ref3,
                       cbl_refer_t& ref4 );

void
parser_string_overflow( cbl_label_t *name );
void
parser_string_overflow_end( cbl_label_t *name );

void
parser_string(  cbl_refer_t tgt,
                cbl_refer_t pointer,
                size_t nsource,
                cbl_string_src_t *sources,
                cbl_label_t *overflow,
                cbl_label_t *not_overflow );

void
parser_unstring( cbl_refer_t src,
                 size_t ndelimited,
                 cbl_refer_t *delimiteds,
                 // into
                 size_t noutput,
                 cbl_refer_t *outputs,
                 cbl_refer_t *delimiters,
                 cbl_refer_t *counts,
                 cbl_refer_t pointer,
                 cbl_refer_t tally,
                 cbl_label_t *overflow,
                 cbl_label_t *not_overflow );

void parser_return_start( cbl_file_t *file, cbl_refer_t into );
void parser_return_atend( cbl_file_t *file );
void parser_return_notatend( cbl_file_t *file );
void parser_return_finish( cbl_file_t *file );

void parser_exception_prepare( const cbl_name_t statement_name,
                               const cbl_enabled_exceptions_array_t *enabled );

//void parser_exception_condition( cbl_field_t *ec );

struct cbl_exception_file;
struct cbl_exception_files_t;

void parser_exception_raise(ec_type_t ec);

void parser_call_exception( cbl_label_t *name );
void parser_call_exception_end( cbl_label_t *name );

//void parser_stash_exceptions(const cbl_enabled_exceptions_array_t *enabled);

void parser_match_exception(cbl_field_t *index,
                            cbl_field_t *blob);
void parser_check_fatal_exception();
void parser_clear_exception();

void parser_call_targets_dump();
size_t parser_call_target_update( size_t caller,
                                  const char extant[],
                                  const char mangled_tgt[] );

void parser_file_stash( struct cbl_file_t *file );

void parser_call( cbl_refer_t name,
                  cbl_refer_t returning,
                  size_t narg, cbl_ffi_arg_t args[],
                  cbl_label_t *except,
                  cbl_label_t *not_except,
                  bool is_function);

void parser_entry_activate( size_t iprog, const cbl_label_t *declarative );

void parser_entry( cbl_field_t *name,
                   size_t narg = 0, cbl_ffi_arg_t args[] = NULL);

bool is_ascending_key(cbl_refer_t key);

void register_main_switch(const char *main_string);

tree parser_cast_long(tree N);
void parser_print_long(tree N);
void parser_print_long(const char *fmt, tree N);
void parser_print_long(long N);
void parser_print_long(const char *fmt, long N); // fmt needs to have a %ls in it
void parser_print_string(const char *ach);
void parser_print_string(const char *fmt, const char *ach); // fmt needs to have a %s in it
void parser_set_statement(const char *statement);
void parser_set_handled(ec_type_t ec_handled);
void parser_set_file_number(int file_number);
void parser_exception_clear();

void parser_init_list_size(int count_of_variables);
void parser_init_list_element(cbl_field_t *field);
void parser_init_list();

tree file_static_variable(tree type, const char *name);

void parser_statement_begin();

#endif
