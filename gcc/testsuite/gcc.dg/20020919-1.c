/* Copyright (C) 2002  Free Software Foundation.
   by Hans-Peter Nilsson  <hp@axis.com>

   Making sure that asm clobbers conflicting with asm-declared input
   operands are detected: ``You may not write a clobber description in a
   way that overlaps with an input or output operand''.

   You must be this tall ---> fit two long longs in asm-declared registers
   to enter this amusement.  */

/* { dg-do compile { target alpha*-*-* cris-*-* crisv32-*-* i?86-*-* mmix-*-* powerpc*-*-* rs6000-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

/* Constructed examples; input/output (same register), output, input, and
   input and output (different registers).  */

/* The long longs are used to test overlap for multi-register
   registers.  REG2 and REG4 must be the second halves (defined as
   higher-numbered parts) of REG1 and REG3 respectively when two registers
   are needed.  */

#if defined (__alpha__)
# define REG1 "$1"
# define REG2 "$2"
#elif defined (__CRIS__)
# define REG1 "r10"
# define REG2 "r11"
# define REG3 "r12"
# define REG4 "r13"
# define REG5 "r9"
#elif defined (__i386__)
# define REG1 "%eax"
# define REG2 "%edx"
/* On Darwin -fpic is on by default, so don't use %ebx. */
# define REG3 "%esi"
# define REG4 "%edi"
#elif defined (__MMIX__)
# define REG1 "$8"
# define REG2 "$9"
#elif defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) \
	|| defined (__POWERPC__) || defined (PPC) || defined (_IBMR2) \
	|| defined (__ppc)
# define REG1 "6"
# define REG2 "7"
# if !defined(_ARCH_PPC64)
#  define REG3 "8"
#  define REG4 "9"
# endif
#elif defined (__x86_64__)
# define REG1 "rax"
# define REG2 "rdx"
#endif

/* For readability of the tests.  */
#ifdef REG3
# define REG1a REG1
# define REG1b REG2
# define REG2a REG3
# define REG2b REG4
#else
# define REG1a REG1
# define REG1b REG1
# define REG2a REG2
# define REG2b REG2
#endif

/* REG5 is just another reg if there is one; the difference to REG4 is to
   keep the original set of registers for CRIS.  */
#ifndef REG5
#define REG5 REG2b
#endif

void *
foo (void *p)
{
  register void *q asm (REG1) = p;
  asm ("foo1 %0" : "=r" (q) : "0" (q) : REG1); /* { dg-error "conflict" } */
  return q;
}

void *
nfoo (void *p)
{
  register void *q asm (REG1) = p;
  asm ("foo1 %0" : "=r" (q) : "0" (q) : REG2);
  return q;
}

long long
foolla (long long llp)
{
  register long long ll asm (REG1a) = llp;
  asm ("foo1a %0" : "=r" (ll) : "0" (ll) : REG1a); /* { dg-error "conflict" } */
  return ll;
}

long long
nfoolla (long long llp)
{
  register long long ll asm (REG1a) = llp;
  asm ("foo1a %0" : "=r" (ll) : "0" (ll) : REG2a);
  return ll;
}

long long
foollb (long long llp)
{
  register long long ll asm (REG1a) = llp;
  asm ("foo1b %0" : "=r" (ll) : "0" (ll) : REG1b); /* { dg-error "conflict" } */
  return ll;
}

void *
bar (void *p)
{
  register void *q asm (REG1);
  register void *w asm (REG2) = p;
  asm ("bar1 %1,%0" : "=r" (q) : "r" (w) : REG1); /* { dg-error "conflict" } */
  return q;
}

long long
barlla (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm asm (REG2a) = llp;
  asm ("bar1a %1,%0" : "=r" (ll) : "r" (mm) : REG1b); /* { dg-error "conflict" } */
  return ll;
}

long long
barllb (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm asm (REG2a) = llp;
  asm ("bar1b %1,%0" : "=r" (ll) : "r" (mm) : REG2b); /* { dg-error "conflict" } */
  return ll;
}

void *
foobar (void *p)
{
  register void *q asm (REG1);
  register void *w asm (REG2) = p;
  asm ("foobar1 %1,%0" : "=r" (q) : "r" (w) : REG2); /* { dg-error "conflict" } */
  return q;
}

void *
nfoobar (void *p)
{
  register void *q asm (REG1);
  register void *w = p;
  asm ("foobar1 %1,%0" : "=r" (q) : "r" (w) : REG2);
  return q;
}

long long
foobarlla (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm asm (REG2a) = llp;
  asm ("foobar1a %1,%0" : "=r" (ll) : "r" (mm) : REG1b); /* { dg-error "conflict" } */
  return ll;
}

long long
nfoobarlla (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm = llp;
  asm ("foobar1a %1,%0" : "=r" (ll) : "r" (mm) : REG2a);
  return ll;
}

long long
foobarllb (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm asm (REG2a) = llp;
  asm ("foobar1b %1,%0" : "=r" (ll) : "r" (mm) : REG2b); /* { dg-error "conflict" } */
  return ll;
}

long long
nfoobarllb (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm = llp;
  asm ("foobar1b %1,%0" : "=r" (ll) : "r" (mm) : REG2b);
  return ll;
}

void *
baz (void *p)
{
  register void *q asm (REG1);
  register void *w asm (REG2) = p;
  asm ("baz1 %1,%0" : "=r" (q) : "r" (w) : REG1, REG2); /* { dg-error "conflict" } */
  return q;
}

void *
nbaz (void *p)
{
  register void *q;
  register void *w = p;
  asm ("baz1 %1,%0" : "=r" (q) : "r" (w) : REG1, REG2);
  return q;
}

void *
nbaz2 (void *p)
{
  register void *q asm (REG1);
  register void *w asm (REG2) = p;
  asm ("baz1 %1,%0" : "=r" (q) : "r" (w));
  return q;
}

long long
bazlla (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm asm (REG2a) = llp;
  asm ("baz1a %1,%0" : "=r" (ll) : "r" (mm) : REG1a, REG2a); /* { dg-error "conflict" } */
  return ll;
}

long long
bazllb (long long llp)
{
  register long long ll asm (REG1a);
  register long long mm asm (REG2a) = llp;
  asm ("baz2a %1,%0" : "=r" (ll) : "r" (mm) : REG1b, REG2b); /* { dg-error "conflict" } */
  return ll;
}

/* Real-world example of bug.  */

#ifdef _WIN64
typedef unsigned int loc_size_t __attribute__ ((mode (DI)));
#else
typedef __SIZE_TYPE__ loc_size_t;
#endif

struct stat;
int
_dl_stat (const char *file_name, struct stat *buf)
{
  register long a asm (REG1) = (long) (loc_size_t) file_name;
  register long b asm (REG2) = (long) (loc_size_t) buf;

  asm volatile ("movu.w %1,$r9\n\tbreak 13" : "=r" (a) : "g" (106), "0" (a), "r" (b) : REG1, REG5); /* { dg-error "conflict" } */
  if (a >= 0)
    return (int) a;
  return (int) -1;
}
