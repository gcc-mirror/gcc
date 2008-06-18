/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "-fno-common" } */

/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1a" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1b" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1c" } } */
/* { dg-final { scan-assembler-not "weak\[^ \t\]*\[ \t\]_?ffoo1d" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1e" } } */

/* test function addresses with #pragma weak */

#pragma weak ffoo1a
extern void * ffoo1a (void);
void * foo1a (void)
{
  return (void *)ffoo1a;
}

extern void * ffoo1b (void);
#pragma weak ffoo1b
void * foo1b (void)
{
  return (void *)ffoo1b;
}

extern void * ffoo1c (void);
void * foo1c (void)
{
  return (void *)ffoo1c;
}
#pragma weak ffoo1c


int ffoo1d (void);
#pragma weak ffoo1d


extern void * ffoo1e (void);
#pragma weak ffoo1e
void * foo1e (void)
{
  if (ffoo1e)
    ffoo1e ();
  return 0;
}

