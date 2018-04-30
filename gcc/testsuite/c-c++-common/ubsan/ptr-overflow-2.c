/* PR sanitizer/80998 */
/* { dg-do run } */
/* { dg-options "-fsanitize=pointer-overflow -fsanitize-recover=pointer-overflow -fno-ipa-icf -Wall" } */

__attribute__((noinline, noclone)) char * f1 (char *p) { return p + 1; }
__attribute__((noinline, noclone)) char * f2 (char *p) { return p - 1; }
__attribute__((noinline, noclone)) char * f3 (char *p, int i) { return p + i; }
__attribute__((noinline, noclone)) char * f4 (char *p, int i) { return p + i; }
__attribute__((noinline, noclone)) char * f5 (char *p, int i) { return p - i; }
__attribute__((noinline, noclone)) char * f6 (char *p, int i) { return p - i; }
__attribute__((noinline, noclone)) char * f7 (char *p, unsigned long int i) { return p + i; }
__attribute__((noinline, noclone)) char * f8 (char *p, unsigned long int i) { return p + i; }
__attribute__((noinline, noclone)) char * f9 (char *p, unsigned long int i) { return p - i; }
__attribute__((noinline, noclone)) char * f10 (char *p, unsigned long int i) { return p - i; }
struct S { int a; int b; int c[64]; };
__attribute__((noinline, noclone)) int *f11 (struct S *p) { return &p->a; }
__attribute__((noinline, noclone)) int *f12 (struct S *p) { return &p->b; }
__attribute__((noinline, noclone)) int *f13 (struct S *p) { return &p->c[64]; }
__attribute__((noinline, noclone)) int *f14 (struct S *p, int i) { return &p->c[i]; }
__attribute__((noinline, noclone)) int *f15 (struct S *p, int i) { return &p->c[i]; }
__attribute__((noinline, noclone)) int *f16 (struct S *p) { return &p->a; }
__attribute__((noinline, noclone)) int *f17 (struct S *p) { return &p->b; }
__attribute__((noinline, noclone)) int *f18 (struct S *p) { return &p->c[64]; }
__attribute__((noinline, noclone)) int *f19 (struct S *p, int i) { return &p->c[i]; }
__attribute__((noinline, noclone)) int *f20 (struct S *p, int i) { return &p->c[i]; }
__attribute__((noinline, noclone)) int *f21 (struct S *p) { return &p->a; }
__attribute__((noinline, noclone)) int *f22 (struct S *p) { return &p->b; }
__attribute__((noinline, noclone)) int *f23 (struct S *p) { return &p->c[64]; }
__attribute__((noinline, noclone)) int *f24 (struct S *p, int i) { return &p->c[i]; }
__attribute__((noinline, noclone)) int *f25 (struct S *p, int i) { return &p->c[i]; }

char *volatile p;
__UINTPTR_TYPE__ volatile u;
struct S *volatile q;
int *volatile r;

int
main ()
{
  u = ~(__UINTPTR_TYPE__) 0;
  p = (char *) u;
  p = f1 (p);
  u = 0;
  p = (char *) u;
  p = f2 (p);
  u = -(__UINTPTR_TYPE__) 7;
  p = (char *) u;
  p = f3 (p, 7);
  u = 3;
  p = (char *) u;
  p = f4 (p, -4);
  u = 23;
  p = (char *) u;
  p = f5 (p, 27);
  u = -(__UINTPTR_TYPE__) 15;
  p = (char *) u;
  p = f6 (p, -15);
  u = -(__UINTPTR_TYPE__) 29;
  p = (char *) u;
  p = f7 (p, 31);
  u = 23;
  p = (char *) u;
  p = f9 (p, 24);
  if (sizeof (unsigned long) < sizeof (char *))
    return 0;
  u = 7;
  p = (char *) u;
  p = f8 (p, -8);
  u = -(__UINTPTR_TYPE__) 25;
  p = (char *) u;
  p = f10 (p, -25);
  u = ~(__UINTPTR_TYPE__) 0;
  q = (struct S *) u;
  r = f11 (q);
  r = f12 (q);
  r = f13 (q);
  r = f14 (q, 0);
  r = f15 (q, 63);
  u = ~(__UINTPTR_TYPE__) 0 - (17 * sizeof (int));
  q = (struct S *) u;
  r = f16 (q);
  r = f17 (q);
  r = f18 (q);
  r = f19 (q, 0);
  r = f20 (q, 63);
  u = 3 * sizeof (int);
  q = (struct S *) u;
  r = f21 (q);
  r = f22 (q);
  r = f23 (q);
  r = f24 (q, -2);
  r = f25 (q, -6);
  return 0;
}

/* { dg-output ":5:6\[79]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+ overflowed to (0\[xX])?0\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:6:6\[79]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?0\+ overflowed to (0\[xX])?\[fF]\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:7:7\[46]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+9 overflowed to (0\[xX])?0\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:8:7\[46]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?0\+3 overflowed to (0\[xX])?\[fF]\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:9:7\[46]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?0\+17 overflowed to (0\[xX])?\[fF]\+\[cC](\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:10:7\[46]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+1 overflowed to (0\[xX])?0\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:11:\[89]\[80]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+\[eE]3 overflowed to (0\[xX])?0\+2(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:13:\[89]\[80]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?0\+17 overflowed to (0\[xX])?\[fF]\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:12:\[89]\[80]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?0\+7 overflowed to (0\[xX])?\[fF]\+(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*:14:\[89]\[91]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+\[eE]7 overflowed to (0\[xX])?0\+" } */
/* { dg-output "(\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:17:\[67]\[82]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+ overflowed to (0\[xX])?0\+3(\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:18:\[67]\[86]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+ overflowed to (0\[xX])?0\+107(\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:19:\[78]\[52]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+ overflowed to (0\[xX])?0\+7(\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:20:\[78]\[52]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+ overflowed to (0\[xX])?0\+103(\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:23:\[67]\[86]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+\[bB]\[bB] overflowed to (0\[xX])?0\+\[cC]3(\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:25:\[78]\[52]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?\[fF]\+\[bB]\[bB] overflowed to (0\[xX])?0\+\[bB]\[fF](\n|\r\n|\r)" { target int32 } } */
/* { dg-output "\[^\n\r]*:30:\[78]\[52]\[^\n\r]*runtime error: pointer index expression with base (0\[xX])?0\+\[cC] overflowed to (0\[xX])?\[fF]\+\[cC]" { target int32 } } */
