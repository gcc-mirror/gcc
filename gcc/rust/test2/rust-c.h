/* rust-c.h -- Header file for Rust frontend gcc C interface. 

Copyright stuff, whatever */

#ifndef RUST_RUST_C_H
#define RUST_RUST_C_H

#define RUST_EXTERN_C

// re-enable if rust versions of these are required
//class Linemap;
//class Backend;

/* Functions defined in the Rust frontend proper called by the GCC
   interface. TODO move around */

extern int rust_enable_dump(const char*);
extern int rust_enable_optimize(const char*, int);

extern void grs_add_search_path(const char*);

struct rust_create_rustrust_args {
    int int_type_size;
    int pointer_size;
    const char* pkgpath;
    const char* prefix;
    const char* relative_import_path;
    const char* c_header;
    //Backend* backend;
    //Linemap* linemap;
    bool check_divide_by_zero;
    bool check_divide_overflow;
    bool compiling_runtime;
    int debug_escape_level;
    const char* debug_escape_hash;
    int64_t nil_check_size_threshold;
    bool debug_optimization;
};

extern void rust_create_rustrust(const struct rust_create_rustrust_args*);

extern void grs_parse_input_files(const char**, unsigned int, bool only_check_syntax, 
    bool require_return_statement);
extern void rust_write_globals(void);

/* Functions defined in the GCC interface called by the Rust frontend
   proper. TODO: move around */

extern void grs_preserve_from_gc(tree);

extern bool saw_errors(void);

extern const char* rust_localize_identifier(const char*);

extern unsigned int rust_field_alignment(tree);

extern void rust_imported_unsafe(void);

extern void rust_write_export_data(const char*, unsigned int);

extern const char* rust_read_export_data(int, off_t, char**, size_t*, int*);

extern GTY(()) tree rust_non_zero_struct;

#endif /* !defined(RUST_RUST_C_H) */
