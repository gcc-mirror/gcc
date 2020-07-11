/* PR middle-end/92341 - missing -Warray-bounds indexing past the end
   of a compound literal
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#include "range.h"

#define INT_MAX    __INT_MAX__
#define INT_MIN    (-__INT_MAX__ - 1)

void sink (int, ...);


#define T(...) sink (__LINE__, (__VA_ARGS__))


void direct_idx_cst (void)
{
  T ((int[]){ }[-1]);           // { dg-warning "array subscript -1 is outside array bounds of 'int\\\[0]'" }
  T ((int[]){ }[0]);            // { dg-warning "array subscript 0 is outside array bounds of 'int\\\[0]'" }
  T ((int[]){ }[1]);            // { dg-warning "array subscript 1 is outside array bounds of 'int\\\[0]'" }

  T ((int[]){ 1 }[-1]);         // { dg-warning "array subscript -1 is below array bounds of 'int\\\[1]'" }
  T ((int[]){ 1 }[0]);
  T ((int[]){ 1 }[1]);          // { dg-warning "array subscript 1 is above array bounds of 'int\\\[1]'" }
  T ((int[]){ 1 }[INT_MIN]);    // { dg-warning "array subscript -\[0-9\]+ is below array bounds of 'int\\\[1]'" }
  T ((int[]){ 1 }[INT_MAX]);    // { dg-warning "array subscript \[0-9\]+ is above array bounds of 'int\\\[1]'" }
  T ((int[]){ 1 }[SIZE_MAX]);   // { dg-warning "array subscript \[0-9\]+ is above array bounds of 'int\\\[1]'" }
}


void direct_idx_var (int i)
{
  T ((char[]){ }[i]);           // { dg-warning "array subscript i is outside array bounds of 'char\\\[0]'" }
  T ((int[]){ }[i]);            // { dg-warning "array subscript i is outside array bounds of 'int\\\[0]'" }
}


void direct_idx_range (void)
{
  ptrdiff_t i = SR (-2, -1);

  T ((int[]){ 1 }[i]);          // { dg-warning "array subscript \[ \n\r]+ is outside array bounds of 'int\\\[0]'" "pr?????" { xfail *-*-* } }
}


#undef T
#define T(idx, ...) do {			\
    int *p = (__VA_ARGS__);			\
    sink (p[idx]);				\
  } while (0)

void ptr_idx_cst (void)
{
  T (-1, (int[]){ });           // { dg-warning "array subscript -1 is outside array bounds of 'int\\\[0]'" }
  T ( 0, (int[]){ });           // { dg-warning "array subscript 0 is outside array bounds of 'int\\\[0]'" }
  T (+1, (int[]){ });           // { dg-warning "array subscript 1 is outside array bounds of 'int\\\[0]'" }

  T (-1, (int[]){ 1 });         // { dg-warning "array subscript -1 is outside array bounds of 'int\\\[1]'" }
  T ( 0, (int[]){ 1 });
  T (+1, (int[]){ 1 });         // { dg-warning "array subscript 1 is outside array bounds of 'int\\\[1]'" }
  T (INT_MIN, (int[]){ 1 });    // { dg-warning "array subscript -\[0-9\]+ is outside array bounds of 'int\\\[1]'" "lp64" { xfail ilp32 } }
  T (INT_MAX, (int[]){ 1 });    // { dg-warning "array subscript \[0-9\]+ is outside array bounds of 'int\\\[1]'" "lp64" { target lp64 } }
                                // { dg-warning "array subscript -1 is outside array bounds of 'int\\\[1]'" "ilp32" { target ilp32 } .-1 }
  T (SIZE_MAX, (int[]){ 1 });   // { dg-warning "array subscript -?\[0-9\]+ is outside array bounds of 'int\\\[1]'" }
}


void ptr_idx_var (int i)
{
  T (i, (int[]){ });            // { dg-warning "array subscript \[^\n\r\]+ is outside array bounds of 'int\\\[0]'" }
  T (i, (int[]){ 1 });
  T (i, (int[]){ i, 1 });
}

void ptr_idx_range (void)
{
  ptrdiff_t i = SR (-2, -1);

  T (i, (int[]){ });            // { dg-warning "array subscript \\\[-2, -1] is outside array bounds of 'int\\\[0]'" }
  T (i, (int[]){ 1 });          // { dg-warning "array subscript \\\[-2, -1] is outside array bounds of 'int\\\[1]'" }
  T (i, (int[]){ i });          // { dg-warning "array subscript \\\[-2, -1] is outside array bounds of 'int\\\[1]'" }

  i = SR (0, 1);

  T (i, (int[]){ });            // { dg-warning "array subscript \\\[0, 1] is outside array bounds of 'int\\\[0]'" }
  T (i, (int[]){ 1 });

  i = SR (1, 2);
  T (i, (int[]){ 1 });          // { dg-warning "array subscript \\\[1, 2] is outside array bounds of 'int\\\[1]'" }

  i = SR (2, 3);
  T (i, (int[]){ 1, 2, 3 });

  i = SR (3, 4);
  T (i, (int[]){ 2, 3, 4 });          // { dg-warning "array subscript \\\[3, 4] is outside array bounds of 'int\\\[3]'" }
}

/* Some of the invalid accesses above also trigger -Wuninitialized.
   { dg-prune-output "\\\[-Wuninitialized" }  */
