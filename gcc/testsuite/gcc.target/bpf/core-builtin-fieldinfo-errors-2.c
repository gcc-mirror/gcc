/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

struct F {
  int bar;
  char c;
  int baz;
};

enum {
  FIELD_BYTE_OFFSET = 0,
  FIELD_BYTE_SIZE = 1,
};

int test (struct F *f) {
  int a;
  unsigned x = __builtin_preserve_field_info (({ a = f->bar + f->baz; }), FIELD_BYTE_OFFSET); /* { dg-error "argument is not a field access" } */

  int b;
  unsigned y = __builtin_preserve_field_info (&(f->c), FIELD_BYTE_SIZE); /* { dg-error "argument is not a field access" } */

  return a + b + x + y;
}
