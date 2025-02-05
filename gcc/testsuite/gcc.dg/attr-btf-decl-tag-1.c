/* Test btf_decl_tag attribute argument checking.  */
/* { dg-do compile } */

void *vptr __attribute__((btf_decl_tag("vptr"), btf_decl_tag ("perthread")));

struct Foo
{
  int x __attribute__((btf_decl_tag (0x55))); /* { dg-error "requires a string" } */
  char *c __attribute__((btf_decl_tag (L"Lstr"))); /* { dg-error "unsupported wide string" } */
};

extern int foo (int x, int y __attribute__((btf_decl_tag))); /* { dg-error "wrong number of arguments" } */

char *str __attribute__((btf_decl_tag("A", "B"))); /* { dg-error "wrong number of arguments" } */
