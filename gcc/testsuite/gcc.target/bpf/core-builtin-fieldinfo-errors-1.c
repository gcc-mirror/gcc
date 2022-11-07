/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct F {
  int bar;
  char c;
  int baz;
  int arr[];
};

enum {
  FIELD_BYTE_OFFSET = 0,
  FIELD_BYTE_SIZE = 1,
};

unsigned int test (struct F *f) {

  unsigned x = __builtin_preserve_field_info (f->arr, FIELD_BYTE_SIZE); /* { dg-error "unsupported variable size field access" } */

  unsigned y = __builtin_preserve_field_info (f->baz, 99); /* { dg-error "invalid second argument to built-in function" } */

  return x + y;
}
