/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */
/* { dg-options "-O2 -fpic -mregparm=3" { target i?86-*-* } } */

extern __thread int i, j, k;
extern void bar(int *, int *, int *);
void foo(void)
{
  bar(&i, &j, &k);
}
