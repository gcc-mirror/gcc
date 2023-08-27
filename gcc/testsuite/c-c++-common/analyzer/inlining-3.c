/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"
typedef __SIZE_TYPE__ size_t;


struct input_file_st 
{
  char inpname[1];
};

typedef struct input_file_st input_file;

static inline const char*
get_input_file_name (const input_file *inpf)
{
  if (inpf)
    /* { dg-message "following 'false' branch \\(when 'inpf' is NULL\\)\\.\\.\\. \\(fndecl 'get_input_file_name', depth 2\\)" "" { target c } .-1 } */
    /* { dg-message "following 'false' branch \\(when 'inpf' is NULL\\)\\.\\.\\. \\(fndecl 'const char\\* get_input_file_name\\(const input_file\\*\\)', depth 2\\)" "" { target c++ } .-2 } */
    return inpf->inpname;
  return NULL;
}

size_t
test (const input_file *inpf)
{
  const char *f = get_input_file_name (inpf);
  return __builtin_strlen (f); /* { dg-warning "use of NULL" "warning" } */
  /* { dg-message "NULL where non-null expected \\(fndecl 'test', depth 1\\)" "message" { target c } .-1 } */
  /* { dg-message "NULL where non-null expected \\(fndecl 'size_t test\\(const input_file\\*\\)', depth 1\\)" "message" { target c++  } .-2 } */
}
