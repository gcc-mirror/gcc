/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */
/* { dg-options "-O2 -fpic -mregparm=3" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target fpic } */

extern __thread int i, j, k;
extern void bar(int *, int *, int *);
void foo(void)
{
  bar(&i, &j, &k);
}
