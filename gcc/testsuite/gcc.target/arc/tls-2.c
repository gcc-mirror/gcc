/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-skip-if "" { arc*-*-elf* } } */
/* { dg-options "-O2" } */

typedef int type_a;
__thread int b;
int c;

extern int bar (char *, int, int *, int, int *, char, type_a, int *, int *);
int foo (int *f, char buffer, type_a buflen, int *g)
{
  bar("", c, (int *)foo, 1, (int *)f, buffer, buflen, (int *)g, &b);
}
