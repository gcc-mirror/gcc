/* Test btf_decl_tag attribute argument checking for wide string types.  */
/* { dg-do compile } */
/* { dg-options "--std=c11" } */

int **my_ptr __attribute__((btf_decl_tag("my_ptr")));

void *x __attribute__((btf_decl_tag (U"Ustr"))); /* { dg-error "unsupported wide string" } */

const int y __attribute__((btf_decl_tag (u"ustr"))); /* { dg-error "unsupported wide string" } */

union U
{
  int x;
  char c __attribute__((btf_decl_tag (u8"u8str"))); /* OK.  */
};
