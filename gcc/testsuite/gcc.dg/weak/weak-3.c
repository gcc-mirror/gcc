/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-require-weak "" } */
/* { dg-options "-fno-common -Waddress" } */
/* { dg-skip-if "" { x86_64-*-mingw* } } */
/* { dg-skip-if PR119369 { amdgcn-*-* } } */

/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1a" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1b" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1c" } } */
/* { dg-final { scan-assembler-not "weak\[^ \t\]*\[ \t\]_?ffoo1d" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1e" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1f" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?ffoo1g" } } */

/* test function addresses with __attribute__((weak)) */

extern void * ffoo1a (void) __attribute__((weak));
extern void * ffoo1a (void);
void * foo1a (void)
{
  return (void *)ffoo1a;
}


extern void * ffoo1b (void);
extern void * ffoo1b (void) __attribute__((weak));
void * foo1b (void)
{
  return (void *)ffoo1b;
}


extern void * ffoo1c (void);  
void * foo1c (void)
{
  return (void *)ffoo1c;
}
extern void * ffoo1c (void) __attribute__((weak));


int ffoo1d (void);
int ffoo1d (void) __attribute__((weak));


extern void * ffoo1e (void);
extern void * ffoo1e (void)  __attribute__((weak));
void * foo1e (void)
{
  if (ffoo1e)
    ffoo1e ();
  return 0;
}


extern void * ffoo1f (void);    
void * foo1f (void)
{
  if (ffoo1f) /* { dg-warning "-Waddress" } */
    ffoo1f ();
  return 0;
}
void * ffoox1f (void) { return (void *)0; }
extern void * ffoo1f (void)  __attribute__((weak, alias ("ffoox1f")));


extern void * ffoo1g (void);
void * ffoox1g (void) { return (void *)0; }
extern void * ffoo1g (void)  __attribute__((weak, alias ("ffoox1g")));
void * foo1g (void)
{
  /* ffoo1g is a weak alias for a symbol defined in this file, expect
     a -Waddress for the test (which is folded to true).  */
  if (ffoo1g)       // { dg-warning "-Waddress" }
    ffoo1g ();
  return 0;
}
