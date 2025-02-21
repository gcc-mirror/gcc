/* PR middle-end/82608 - missing -Warray-bounds on an out-of-bounds VLA index
 { dg-do compile }
 { dg-options "-O2 -Wall -Wno-uninitialized -ftrack-macro-expansion=0" } */

#include "range.h"

typedef __INT16_TYPE__ int16_t;

#define alloca(n) __builtin_alloca (n)

void* calloc (size_t, size_t);
void* malloc (size_t);

void sink (void*, ...);
#define sink(...) sink (0, __VA_ARGS__)

#define T(x) (sink (x))

__attribute__ ((alloc_size (1))) void* alloc (size_t);


void test_alloca_cst (void)
{
  {
    char *p = alloca (1);
    sink (p);
    T (p[0]);
    T (p[1]);                 // { dg-warning "subscript 1 is outside array bounds of 'char\\\[1\\\]'" }
  }

  {
    char *p = alloca (2);
    sink (p);
    T (p[0]), T (p[1]);
    T (p[2]);                 // { dg-warning "subscript 2 is outside array bounds of 'char\\\[2\\\]'" }
  }

  {
    char *p = alloca (3);
    sink (p);
    T (p[0]), T (p[1]), T (p[2]);
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" }
  }
}


void test_alloca_char_range (int i, unsigned n, size_t sz)
{
  {
    // Be sure to exercise signed as well as unsigned arguments.
    char *p = alloca (i);
    sink (p);
    T (p[0]), T (p[1]), T (p[12345]);
    T (p[-1]);                // { dg-warning "subscript -1 is outside array bounds of 'char\\\[" }
  }

  {
    char *p = alloca (n);
    sink (p);
    T (p[0]), T (p[1]), T (p[12345]);
    T (p[-1]);                // { dg-warning "subscript -1 is outside array bounds of 'char\\\[" }
  }

  {
    char *p = alloca (sz);
    sink (p);
    T (p[0]), T (p[1]), T (p[23456]);
    T (p[-1]);                // { dg-warning "subscript -1 is outside array bounds of 'char\\\[" }
  }

  {
    char *p = alloca (UR (0, 1));
    sink (p);
    T (p[0]);
    T (p[1]);                 // { dg-warning "subscript 1 is outside array bounds of 'char\\\[1\\\]'" }
  }

  {
    char *p = alloca (UR (0, 2));
    sink (p);
    sink (p[0], p[1]);
    sink (p[2]);              // { dg-warning "subscript 2 is outside array bounds of 'char\\\[2\\\]'" }
  }

  {
    char *p = alloca (UR (0, 3));
    sink (p);
    T (p[0]), T (p[1]), T (p[2]);
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" }
  }

  {
    char *p = alloca (UR (1, 3));
    sink (p);
    T (p[0]), T (p[1]), T (p[2]);
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" }
  }

  {
    char *p = alloca (UR (2, 3));
    sink (p);
    T (p[0]), T (p[1]), T (p[2]);
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" }
  }
}


void test_alloca_int16_range (unsigned n)
{
  int16_t *p;
  {
    p = alloca (n);           // { dg-message "allocated by " }
    sink (p);
    T (p[0]), T (p[1]), T (p[12345]);
    T (p[-1]);                // { dg-warning "subscript -1 is outside array bounds of 'int16_t\\\[" }
  }

  {
    p = alloca (UR (0, 1));   // { dg-message "at offset \\d+ into object of size \\\[0, 1] allocated by '__builtin_alloca'" "note" }
    sink (p);
    T (p[0]);                 // { dg-warning "subscript 'int16_t {aka short int}\\\[0\\\]' is partly outside array bounds of 'unsigned char\\\[1]'" }
    T (p[1]);                 // { dg-warning "subscript 1 is outside array bounds of 'int16_t\\\[0]'" }
  }

  {
    p = alloca (UR (0, 2));   // { dg-message "at offset \\d+ into object of size \\\[0, 2] allocated by '__builtin_alloca'" "note" }
    sink (p);
    sink (p[0]);
    sink (p[1]);              // { dg-warning "subscript 1 is outside array bounds of 'int16_t\\\[1]'" }
    sink (p[2]);              // { dg-warning "subscript 2 is outside array bounds of 'int16_t\\\[1\\\]'" }
  }

  {
    p = alloca (UR (0, 3));   // { dg-message "at offset \\d+ into object of size \\\[0, 3] allocated by '__builtin_alloca'" "note" }
    sink (p);
    T (p[0]);
    T (p[1]);                 // { dg-warning "subscript 'int16_t {aka short int}\\\[1\\\]' is partly outside array bounds of 'unsigned char\\\[3]'" }
    T (p[2]);                 // { dg-warning "subscript 2 is outside array bounds of 'int16_t\\\[1\\\]'" }
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'int16_t\\\[1\\\]'" }
  }

  {
    p = alloca (UR (1, 3));    // { dg-message "at offset 1|2|3 into object of size \\\[1, 3] allocated by '__builtin_alloca'" "note" }
    sink (p);
    T (p[0]);
    T (p[1]);                 // { dg-warning "subscript 'int16_t {aka short int}\\\[1\\\]' is partly outside array bounds of 'unsigned char\\\[3]'" }
    T (p[2]);                 // { dg-warning "subscript 2 is outside array bounds of 'int16_t\\\[1\\\]'" }
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'int16_t\\\[1\\\]'" }
  }

  {
    p = alloca (UR (2, 3));    // { dg-message "at offset 2|4 into object of size \\\[2, 3] allocated by '__builtin_alloca'" "note" }
    sink (p);
    T (p[0]);
    T (p[1]);                 // { dg-warning "subscript 'int16_t {aka short int}\\\[1\\\]' is partly outside array bounds of 'unsigned char\\\[3]'" }
    T (p[2]);                 // { dg-warning "subscript 2 is outside array bounds of 'int16_t\\\[1\\\]'" }
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'int16_t\\\[1\\\]'" }
  }

  {
    p = alloca (UR (3, 4));    // { dg-message "at offset 4|6 into object of size \\\[3, 4] allocated by '__builtin_alloca'" "note" }
    sink (p);
    T (p[0]);
    T (p[1]);
    T (p[2]);                 // { dg-warning "subscript 2 is outside array bounds of 'int16_t\\\[2\\\]'" }
    T (p[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'int16_t\\\[2\\\]'" }
  }
}


void test_vla_cst (void)
{
  int n = 1;
  {
    char a[n];
    sink (a);
    T (a[0]);
    T (a[1]);                 // { dg-warning "subscript 1 is (above|outside) array bounds " }
  }

  {
    n = 2;
    char a[n];
    sink (a);
    T (a[0]), T (a[1]);
    T (a[2]);                 // { dg-warning "subscript 2 is (above|outside) array bounds " }
  }

  {
    n = 3;
    char a[n], *p = a;
    sink (p);
    T (p[0]), T (p[1]), T (p[2]);
    T (p[3]);                 // { dg-warning "subscript 3 is (above|outside) array bounds " }
  }
}


void test_vla_char_range (int i, unsigned n, size_t sz)
{
  {
    char a[i];
    sink (a);
    T (a[0]), T (a[1]), T (a[12345]);
    T (a[-1]);                // { dg-warning "subscript -1 is (below|outside) array bounds of 'char\\\[" }
  }

  {
    char a[n];
    sink (a);
    T (a[0]), T (a[1]), T (a[12345]);
    T (a[-1]);                // { dg-warning "subscript -1 is (below|outside) array bounds of 'char\\\[" }
  }

  {
    char a[sz];
    sink (a);
    T (a[0]), T (a[1]), T (a[23456]);
    T (a[-1]);                // { dg-warning "subscript -1 is (below|outside) array bounds of 'char\\\[" }
  }

  {
    char a[UR (0, 1)];
    sink (a);
    T (a[0]);
    T (a[1]);                 // { dg-warning "subscript 1 is outside array bounds of 'char\\\[1\\\]'" "pr82608" { xfail *-*-* } }
  }

  {
    char a[UR (0, 2)];
    sink (a);
    sink (a[0], a[1]);
    sink (a[2]);              // { dg-warning "subscript 2 is outside array bounds of 'char\\\[2\\\]'" "pr82608" { xfail *-*-* } }
  }

  {
    char a[UR (0, 3)];
    sink (a);
    T (a[0]), T (a[1]), T (a[2]);
    T (a[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" "pr82608" { xfail *-*-* } }
  }

  {
    char a[UR (1, 3)];
    sink (a);
    T (a[0]), T (a[1]), T (a[2]);
    T (a[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" "pr82608" { xfail *-*-* } }
  }

  {
    char a[UR (2, 3)];
    sink (a);
    T (a[0]), T (a[1]), T (a[2]);
    T (a[3]);                 // { dg-warning "subscript 3 is outside array bounds of 'char\\\[3\\\]'" "pr82608" { xfail *-*-* } }
  }
}
