/* { dg-do compile } */
/* { dg-options "-fno-common" } */

/* COFF does not support weak, and dg doesn't support UNSUPPORTED.  */
/* { dg-do compile { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */

/* { dg-final { global target_triplet } } */
/* { dg-final { if [string match h8300-*-hms $target_triplet ] {return} } } */
/* { dg-final { if [string match i?86-pc-cygwin $target_triplet ] {return} } } */
/* { dg-final { if [string match *-*-coff $target_triplet ] {return} } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1a" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1b" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1c" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1d" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1e" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1f" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1g" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1h" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1i" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1j" } } */
/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]vfoo1k" } } */

/* test variable addresses with #pragma weak */

#pragma weak vfoo1a
extern int vfoo1a;
void * foo1a (void)
{
  return (void *)&vfoo1a;
}


extern int vfoo1b;
#pragma weak vfoo1b
void * foo1b (void)
{
  return (void *)&vfoo1b;
}


extern int vfoo1c;  /* { dg-warning "applying #pragma weak" "applying #pragma weak" { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */
void * foo1c (void)
{
  return (void *)&vfoo1c;
}
#pragma weak vfoo1c


#pragma weak vfoo1d
int vfoo1d;
void * foo1d (void)
{
  return (void *)&vfoo1d;
}


int vfoo1e;
#pragma weak vfoo1e
void * foo1e (void)
{
  return (void *)&vfoo1e;
}


int vfoo1f;
void * foo1f (void)
{
  return (void *)&vfoo1f;
}
#pragma weak vfoo1f


extern int vfoo1g;  /* { dg-warning "applying #pragma weak" "applying #pragma weak" { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */
void * foo1g (void)
{
  return (void *)&vfoo1g;
}
#pragma weak vfoo1g
int vfoo1g;


extern int vfoo1h;
void * foo1h (void)
{
  return (void *)&vfoo1h;
}
int vfoo1h;
#pragma weak vfoo1h


int vfoo1i;
extern int vfoo1i;
void * foo1i (void)
{
  return (void *)&vfoo1i;
}
#pragma weak vfoo1i


extern int vfoo1j;
int vfoo1j;
void * foo1j (void)
{
  return (void *)&vfoo1j;
}
#pragma weak vfoo1j


#pragma weak vfoo1k
int vfoo1k = 1;

