/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times "addq" 16 } } */
/* { dg-final { scan-assembler-times "subq" 16 } } */
/* { dg-final { scan-assembler-times "moveq" 4 } } */
extern int bar (void);

#define FOO(x) \
  void foo##x (void) { int a = bar (); if (a == x) bar (); } \
  void bar##x (void) { int a = bar (); if (a == -x) bar (); } \
  void foon##x (void) { int a = bar (); if (a != x) bar (); } \
  void barn##x (void) { int a = bar (); if (a != -x) bar (); } \
  

FOO (1)
FOO (2)
FOO (3)
FOO (4)
FOO (5)
FOO (6)
FOO (7)
FOO (8)
FOO (9)


