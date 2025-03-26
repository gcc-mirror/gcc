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
#ifndef _GENUTIL_H_
#define _GENUTIL_H_

#define EBCDIC_MINUS (0x60)
#define EBCDIC_PLUS  (0x4E)
#define EBCDIC_ZERO  (0xF0)
#define EBCDIC_NINE  (0xF9)

bool internal_codeset_is_ebcdic();

extern bool exception_location_active;
extern bool skip_exception_processing;

extern bool suppress_dest_depends;

extern std::vector<std::string>current_filename;

extern tree var_decl_exception_code;         // int         __gg__exception_code;
extern tree var_decl_exception_handled;      // int         __gg__exception_handled;
extern tree var_decl_exception_file_number;  // int         __gg__exception_file_number;
extern tree var_decl_exception_file_status;  // int         __gg__exception_file_status;
extern tree var_decl_exception_file_name;    // const char *__gg__exception_file_name;
extern tree var_decl_exception_statement;    // const char *__gg__exception_statement;
extern tree var_decl_exception_source_file;  // const char *__gg__exception_source_file;
extern tree var_decl_exception_line_number;  // int         __gg__exception_line_number;
extern tree var_decl_exception_program_id;   // const char *__gg__exception_program_id;
extern tree var_decl_exception_section;      // const char *__gg__exception_section;
extern tree var_decl_exception_paragraph;    // const char *__gg__exception_paragraph;

extern tree var_decl_default_compute_error;  // int         __gg__default_compute_error;
extern tree var_decl_rdigits;                // int         __gg__rdigits;
extern tree var_decl_odo_violation;          // int         __gg__odo_violation;
extern tree var_decl_unique_prog_id;         // size_t      __gg__unique_prog_id;

extern tree var_decl_entry_location;         // This is for managing ENTRY statements
extern tree var_decl_exit_address;           // This is for implementing pseudo_return_pop

extern tree var_decl_call_parameter_signature; // char   *__gg__call_parameter_signature
extern tree var_decl_call_parameter_count;     // int __gg__call_parameter_count
extern tree var_decl_call_parameter_lengths;   // size_t *var_decl_call_parameter_lengths

extern tree var_decl_return_code;            // short __gg__data_return_code

extern tree var_decl_arithmetic_rounds_size;  // size_t __gg__arithmetic_rounds_size;
extern tree var_decl_arithmetic_rounds;       // int*   __gg__arithmetic_rounds;
extern tree var_decl_fourplet_flags_size;     // size_t __gg__fourplet_flags_size;
extern tree var_decl_fourplet_flags;          // int*   __gg__fourplet_flags;

extern tree var_decl_treeplet_1f; // cblc_field_pp_type_node , "__gg__treeplet_1f"
extern tree var_decl_treeplet_1o; // SIZE_T_P                , "__gg__treeplet_1o"
extern tree var_decl_treeplet_1s; // SIZE_T_P                , "__gg__treeplet_1s"
extern tree var_decl_treeplet_2f; // cblc_field_pp_type_node , "__gg__treeplet_2f"
extern tree var_decl_treeplet_2o; // SIZE_T_P                , "__gg__treeplet_2o"
extern tree var_decl_treeplet_2s; // SIZE_T_P                , "__gg__treeplet_2s"
extern tree var_decl_treeplet_3f; // cblc_field_pp_type_node , "__gg__treeplet_3f"
extern tree var_decl_treeplet_3o; // SIZE_T_P                , "__gg__treeplet_3o"
extern tree var_decl_treeplet_3s; // SIZE_T_P                , "__gg__treeplet_3s"
extern tree var_decl_treeplet_4f; // cblc_field_pp_type_node , "__gg__treeplet_4f"
extern tree var_decl_treeplet_4o; // SIZE_T_P                , "__gg__treeplet_4o"
extern tree var_decl_treeplet_4s; // SIZE_T_P                , "__gg__treeplet_4s"

extern tree var_decl_nop;         // int __gg__nop
extern tree var_decl_main_called; // int __gg__main_called

int       get_scaled_rdigits(cbl_field_t *field);
int       get_scaled_digits(cbl_field_t *field);
tree      tree_type_from_digits(size_t digits, int signable);
tree      tree_type_from_size(size_t bytes, int signable);
tree      tree_type_from_field(cbl_field_t *field);
void      get_binary_value( tree value,
                            tree rdigits,
                            cbl_field_t *field,
                            tree         field_offset,
                            tree         hilo = NULL);
tree      get_data_address( cbl_field_t *field,
                            tree         offset);

FIXED_WIDE_INT(128) get_power_of_ten(int n);
void      scale_by_power_of_ten_N(tree value,
                                int N,
                                bool check_for_fractional = false);
tree      scale_by_power_of_ten(tree value,
                                tree N,
                                bool check_for_fractional = false);
void      scale_and_round(tree value,
                          int  value_rdigits,
                          bool target_is_signable,
                          int  target_rdigits,
                          cbl_round_t rounded);
void      hex_dump(tree data, size_t bytes);
void      set_exception_code_func(ec_type_t ec,
                                  int line,
                                  int from_raise_statement=0);
#define set_exception_code(ec) set_exception_code_func(ec, __LINE__)
bool      process_this_exception(ec_type_t ec);
#define   CHECK_FOR_FRACTIONAL_DIGITS true
void      get_integer_value(tree value,
                            cbl_field_t *field,
                            tree         offset=NULL,  // size_t
                            bool check_for_fractional_digits=false);
void      rt_error(const char *msg);
void      copy_little_endian_into_place(cbl_field_t *dest,
                                        tree         dest_offset,
                                        tree value,
                                        int rhs_rdigits,
                                        bool check_for_error,
                                        tree &size_error);
tree      build_array_of_size_t( size_t  N,
                                 const size_t *values);
void      parser_display_internal_field(tree file_descriptor,
                                        cbl_field_t *field,
                                        bool advance=DISPLAY_NO_ADVANCE);
char     *get_literal_string(cbl_field_t *field);

bool      refer_is_clean(cbl_refer_t &refer);

tree      refer_offset_source(cbl_refer_t &refer,
                              int *pflags=NULL);
tree      refer_size_source(cbl_refer_t &refer);
tree      refer_offset_dest(cbl_refer_t &refer);
tree      refer_size_dest(cbl_refer_t &refer);

void     REFER_CHECK( const char *func,
                      int         line,
                      cbl_refer_t &refer
                      );
#define refer_check(a) REFER_CHECK(__func__, __LINE__, a)

tree      qualified_data_source(cbl_refer_t &refer);

tree      qualified_data_dest(cbl_refer_t &refer);

void      build_array_of_treeplets( int ngroup,
                                    size_t N,
                                    cbl_refer_t *refers);

void      build_array_of_fourplets( int ngroup,
                                    size_t N,
                                    cbl_refer_t *refers);
#endif
