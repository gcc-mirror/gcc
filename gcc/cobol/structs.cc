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
    /*  This module exists in support of genapi.c

        It creates the declarations for structures that are implemented in the
        the libgcobol run-time library.  These are type_decls; the analog in the
        C world would be that these are typedefs:

        typedef struct XXX_
            {
            ....
            } XXX;

        These functions  don't, on their own, allocate any storage.  That gets done
        when the type_decl is handed to the build_decl routine, which creates
        a var_decl.  And that gets added to the GENERIC tree when the var_decl
        is turned into a decl_expr by build1() and then the decl_expr is added
        to the current statement list.

        Your best bet is to simply emulate the code here to create the type_decl
        for each structure, and then just use gg_declare_variable() to create the
        storage when you need it.

        Learning from the code in genapi.c is your best bet.

        */

#include "cobol-system.h"
#include "coretypes.h"
#include "tree.h"
#include "../../libgcobol/ec.h"
#include "../../libgcobol/common-defs.h"
#include "util.h"
#include "cbldiag.h"
#include "symbols.h"
#include "gengen.h"

tree
var_decl_node_p_of( cbl_field_t *var )
    {
    if( var->var_decl_node )
        {
        return gg_get_address_of(var->var_decl_node);
        }
    else
        {
        return null_pointer_node;
        }
    }

// These routines return references, rather than values.  So, in cases
// like MOVE TABLE(a) TO TABLE (b), you need to gg_assign the returned
// value elsewhere, rather than use them directly, because the second
// refer_qualification calculation will overwrite the first.

tree
member(tree var, const char *member_name)
    {
    return gg_struct_field_ref(var, member_name);
    }

tree
member(cbl_field_t *var, const char *member_name)
    {
    return gg_struct_field_ref(var->var_decl_node, member_name);
    }

tree
member(cbl_file_t *var, const char *member_name)
    {
    return gg_struct_field_ref(var->var_decl_node, member_name);
    }

void
member(tree var, const char *member_name, int value)
    {
    gg_assign(  member(var, member_name),
                build_int_cst_type(INT, value) );
    }

void
member(tree var, const char *member_name, tree value)
    {
    gg_assign(  member(var, member_name),
                value );
    }

void
member(cbl_field_t *var, const char *member_name, tree value)
    {
    gg_assign(  member(var->var_decl_node, member_name),
                value );
    }

tree
member2(tree var, const char *member_name, const char *submember)
    {
    tree level1 = member(var,    member_name);
    return        member(level1, submember  );
    }

void
member2(tree var, const char *member_name, const char *submember, int value)
    {
    tree level1 = member(var,    member_name);
    tree level2 = member(level1, submember  );
    gg_assign(level2, build_int_cst_type(INT, value) );
    }

void
member2(tree var, const char *member_name, const char *submember, tree value)
    {
    tree level1 = member(var,    member_name);
    tree level2 = member(level1, submember  );
    gg_assign(level2, value);
    }

void
member3(tree var, const char *mem, const char *sub2, const char *sub3, tree value)
    {
    tree level1 = member(var,    mem);
    tree level2 = member(level1, sub2  );
    tree level3 = member(level2, sub3  );
    gg_assign(level3, value);
    }

tree cblc_field_type_node;
tree cblc_field_p_type_node;
tree cblc_field_pp_type_node;
tree cblc_file_type_node;
tree cblc_file_p_type_node;
tree cbl_enabled_exception_type_node;
tree cblc_goto_type_node;

// The following functions return type_decl nodes for the various structures

static tree
create_cblc_field_t()
    {
    /*
    typedef struct cblc_field_t
        {
        unsigned char *data;        // The runtime data. There is no null terminator
        size_t         capacity;    // The size of "data"
        size_t         allocated;   // The number of bytes available for capacity
        size_t   offset;            // Offset from our ancestor
        char    *name;              // The null-terminated name of this variable
        char    *picture;           // The null-terminated picture string.
        char    *initial;           // The null_terminated initial value
        struct cblc_field_t *parent;// This field's immediate parent field
        size_t occurs_lower;        // non-zero for a table
        size_t occurs_upper;        // non-zero for a table
        uint64_t attr;              // See cbl_field_attr_t
        signed char type;           // A one-byte copy of cbl_field_type_t
        signed char level;          // This variable's level in the naming heirarchy
        signed char digits;         // Digits specified in PIC string; e.g. 5 for 99v999
        signed char rdigits;        // Digits to the right of the decimal point. 3 for 99v999
        } cblc_field_t;
    */
    tree retval = NULL_TREE;
    retval = gg_get_filelevel_struct_type_decl( "cblc_field_t",
                                            16,
                                            UCHAR_P, "data",
                                            SIZE_T,  "capacity",
                                            SIZE_T,  "allocated",
                                            SIZE_T,  "offset",
                                            CHAR_P,  "name",
                                            CHAR_P,  "picture",
                                            CHAR_P,  "initial",
                                            CHAR_P,  "parent",
                                            SIZE_T,  "occurs_lower",
                                            SIZE_T,  "occurs_upper",
                                            ULONGLONG, "attr",
                                            SCHAR,   "type",
                                            SCHAR,   "level",
                                            SCHAR,   "digits",
                                            SCHAR,   "rdigits",
                                            INT,     "dummy");  // Needed to make it an even number of 32-bit ints
    retval = TREE_TYPE(retval);

    return retval;
    }

static tree
create_cblc_file_t()
    {
    // When doing FILE I/O, you need the cblc_file_t structure

    /*
typedef struct cblc_file_t
    {
    char                *name;             // This is the name of the structure; might be the name of an environment variable
    size_t               symbol_index;     // The symbol table index of the related cbl_file_t structure
    char                *filename;         // The name of the file to be opened
    FILE                *file_pointer;     // The FILE *pointer
    cblc_field_t        *default_record;   // The record_area
    size_t               record_area_min;  // The size of the smallest 01 record in the FD
    size_t               record_area_max;  // The size of the largest  01 record in the FD
    cblc_field_t       **keys;             // For relative and indexed files.  The first is the primary key. Null-terminated.
    int                 *key_numbers;      // One per key -- each key has a number. This table is key_number + 1
    int                 *uniques;          // One per key
    cblc_field_t        *password;         //
    cblc_field_t        *status;           // This must exist, and is the cbl_field_t version of io_status
    cblc_field_t        *user_status;      // This might exist, and is another copy See 2014 standard, section 9.1.12
    cblc_field_t        *vsam_status;      //
    cblc_field_t        *record_length;    //
    supplemental_t      *supplemental;     //
    void                *implementation;   // reserved for any implementation
    size_t               reserve;          // From I-O section RESERVE clause
    long                 prior_read_location;   // Location of immediately preceding successful read
    cbl_file_org_t       org;              // from ORGANIZATION clause
    cbl_file_access_t    access;           // from ACCESS MODE clause
    int                  mode_char;        // 'r', 'w', '+', or 'a' from FILE OPEN statement
    int                  errnum;           // most recent errno; can't reuse "errno" as the name
    file_status_t        io_status;        // See 2014 standard, section 9.1.12
    int                  padding;          // Actually a char
    int                  delimiter;        // ends a record; defaults to '\n'.
    int                  flags;            // cblc_file_flags_t
    int                  recent_char;      // This is the most recent char sent to the file
    int                  recent_key;
    cblc_file_prior_op_t prior_op;
    int                  dummy             // We need an even number of INT
    } cblc_file_t;
    */

    tree retval = NULL_TREE;
    retval = gg_get_filelevel_struct_type_decl( "cblc_file_t",
                                            31,
                                            CHAR_P,    "name",
                                            SIZE_T,    "symbol_table_index",
                                            CHAR_P,    "filename",
                                            FILE_P,    "file_pointer",
                                            cblc_field_p_type_node, "default_record",
                                            SIZE_T,    "record_area_min",
                                            SIZE_T,    "record_area_max",
                                            build_pointer_type(cblc_field_p_type_node), "keys",
                                            build_pointer_type(INT),"key_numbers",
                                            build_pointer_type(INT),"uniques",
                                            cblc_field_p_type_node, "password",
                                            cblc_field_p_type_node, "status",
                                            cblc_field_p_type_node, "user_status",
                                            cblc_field_p_type_node, "vsam_status",
                                            cblc_field_p_type_node, "record_length",
                                            VOID_P,                 "supplemental",
                                            VOID_P,                 "implementation",
                                            SIZE_T,    "reserve",
                                            LONG,      "prior_read_location",
                                            INT,       "org",
                                            INT,       "access",
                                            INT,       "mode_char",
                                            INT,       "errnum",
                                            INT,       "io_status",
                                            INT,       "padding",
                                            INT,       "delimiter",
                                            INT,       "flags",
                                            INT,       "recent_char",
                                            INT,       "recent_key",
                                            INT,       "prior_op",
                                            INT,       "dummy");
    retval = TREE_TYPE(retval);
    return retval;
    }

static tree
create_cbl_enabled_exception_t()
    {
    /*
    struct cbl_enabled_exception_t
        {
        bool enabled, location;
        ec_type_t ec;
        size_t file;
        };
    */
    tree retval = NULL_TREE;
    retval = gg_get_filelevel_struct_type_decl( "cbl_enabled_exception_t",
                                            4,
                                            BOOL,   "enabled",
                                            BOOL,   "location",
                                            UINT,   "ec",
                                            SIZE_T, "file");
    retval = TREE_TYPE(retval);

    return retval;
    }

void
create_our_type_nodes()
    {
    static bool just_once = true;
    if( just_once )
        {
        just_once = false;
        cblc_field_type_node              = create_cblc_field_t();
        cblc_field_p_type_node            = build_pointer_type(cblc_field_type_node);
        cblc_field_pp_type_node           = build_pointer_type(cblc_field_p_type_node);
        cblc_file_type_node               = create_cblc_file_t();
        cblc_file_p_type_node             = build_pointer_type(cblc_file_type_node);
        cbl_enabled_exception_type_node   = create_cbl_enabled_exception_t();
        }
    }

