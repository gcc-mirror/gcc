#ifndef GFC_CPP_H
#define GFC_CPP_H

/* Returns true if preprocessing is enabled, false otherwise.  */
bool gfc_cpp_enabled (void);

bool gfc_cpp_preprocess_only (void);

const char *gfc_cpp_temporary_file (void);


void gfc_cpp_init_0 (void);
void gfc_cpp_init (void);

void gfc_cpp_init_options (unsigned int argc, const char **argv);

int gfc_cpp_handle_option(size_t scode, const char *arg, int value);

void gfc_cpp_post_options (void);

try gfc_cpp_preprocess (const char *source_file);

void gfc_cpp_done (void);

void gfc_cpp_add_include_path (char *path, bool user_supplied);

void gfc_cpp_register_include_paths (void);

#endif /* GFC_CPP_H */
