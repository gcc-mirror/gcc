/* { dg-do compile } */
/* { dg-options "-mcmse" }  */

struct span {
  int a, b;
};
struct span2 {
  float a, b, c, d;
};

union test_union
{
  long long a;
  int b;
  struct span2 c;
} test_union;

void __attribute__ ((cmse_nonsecure_entry))
foo (long long a, int b, long long c) {} /* { dg-error "not available to functions with arguments passed on the stack" } */

void __attribute__ ((cmse_nonsecure_entry))
bar (long long a, int b, struct span c) {} /* { dg-error "not available to functions with arguments passed on the stack" } */

void __attribute__ ((cmse_nonsecure_entry))
baz (int a, ...) {} /* { dg-error "not available to functions with variable number of arguments" } */

struct span __attribute__ ((cmse_nonsecure_entry))
qux (void) { /* { dg-error "not available to functions that return value on the stack" } */
  struct span ret = {0, 0};
  return ret;
}

void __attribute__ ((cmse_nonsecure_entry))
norf (struct span2 a) {}

void __attribute__ ((cmse_nonsecure_entry))
foo2 (long long a, int b, union test_union c) {} /* { dg-error "not available to functions with arguments passed on the stack" } */

typedef void __attribute__ ((cmse_nonsecure_call)) bar2 (long long a, int b, long long c); /* { dg-error "not available to functions with arguments passed on the stack" } */

typedef void __attribute__ ((cmse_nonsecure_call)) baz2 (long long a, int b, struct span c); /* { dg-error "not available to functions with arguments passed on the stack" } */

typedef struct span __attribute__ ((cmse_nonsecure_call)) qux2 (void); /* { dg-error "not available to functions that return value on the stack" } */

typedef void __attribute__ ((cmse_nonsecure_call)) norf2 (int a, ...); /* { dg-error "not available to functions with variable number of arguments" } */
