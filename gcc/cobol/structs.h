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
#ifndef STRUCTS_H__
#define STRUCTS_H__

extern tree var_decl_node_p_of( cbl_field_t *var );

// Simple fetch
extern tree member(tree var, const char *member_name);
extern tree member(cbl_field_t *var, const char *member_name);
extern tree member(cbl_refer_t refer, const char *member_name);

extern tree member(cbl_file_t *var, const char *member_name);
extern tree member2(tree var, const char *member_name, const char *submember);

// assignment
extern void member(tree var, const char *member_name, int value);
extern void member(tree var, const char *member_name, tree value);
extern void member(cbl_field_t *var, const char *member_name, tree value);

extern void member2(tree var, const char *member_name, const char *submember, int value);
extern void member2(tree var, const char *member_name, const char *submember, tree value);
extern void member3(tree var, const char *mem, const char *sub1, const char *sub2, tree value);

extern GTY(()) tree cblc_field_type_node;
extern GTY(()) tree cblc_field_p_type_node;
extern GTY(()) tree cblc_field_pp_type_node;
extern GTY(()) tree cblc_file_type_node;
extern GTY(()) tree cblc_file_p_type_node;
extern GTY(()) tree cbl_enabled_exception_type_node;
extern GTY(()) tree cblc_goto_type_node;

extern void create_our_type_nodes();

#endif
