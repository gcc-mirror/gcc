/* { dg-do compile } */
/* { dg-options "-fno-common" } */

/* COFF does not support weak, and dg doesn't support UNSUPPORTED.  */
/* { dg-do compile { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */

/* { dg-final { global target_triplet } } */
/* { dg-final { if [string match h8300-*-hms $target_triplet ] {return} } } */
/* { dg-final { if [string match i?86-pc-cygwin $target_triplet ] {return} } } */
/* { dg-final { if [string match *-*-coff $target_triplet ] {return} } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]ffoo1a" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]ffoo1b" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]ffoo1c" } } */
/* { dg-final { scan-assembler-not "weak\[^ \t\]*\[ \t\]ffoo1d" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]ffoo1e" } } */

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

extern void * ffoo1c (void);  /* { dg-warning "applying #pragma weak" "applying #pragma weak" { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */
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

