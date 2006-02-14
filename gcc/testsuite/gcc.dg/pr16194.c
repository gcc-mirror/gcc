/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-bogus "internal compiler error" "ICE" { target *-*-* } 0 } */

#define ASMDECL __asm (REG);
#define CLOBBER_LIST : REG
#define INP_CLOBBER_LIST : CLOBBER_LIST
#if defined (__alpha__)
# define REG "$1"
#elif defined (__CRIS__) || defined (__sh__)
# define REG "r10"
#elif defined (__i386__)
# define REG "%eax"
#elif defined (__MMIX__)
# define REG "$8"
#elif defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) \
        || defined (__POWERPC__) || defined (PPC) || defined (_IBMR2)
# define REG "6"
#elif defined (__x86_64__)
# define REG "rax"
#else
  /* Make this test harmless for any target not recognized above.  */
# undef ASMDECL
# define ASMDECL
# define REG "conflict"
# undef CLOBBER_LIST
# define CLOBBER_LIST
# undef INP_CLOBBER_LIST
# define INP_CLOBBER_LIST
#endif

struct A
{
  int a;
};

struct B
{
  struct A b[3];
};

struct C
{
  struct B c;
};

void bug (void)
{
  register char* dst ASMDECL;
  __asm__ ("":"=g"(*dst): : REG); /* { dg-error "conflict" } */
}

/* The tree optimizers currently prevent us from finding an overlap -
   we end up using a copy of dst rather than dst.
   But at least make sure we don't get an ICE.  */
void bug2 (void)
{
  register char* dst ASMDECL;
  __asm__ ("": :"g"(*dst) CLOBBER_LIST);
}

void
foo (void)
{
  register struct C *dst ASMDECL;
  __asm__ ("" : "=g"(dst->c.b[1].a) INP_CLOBBER_LIST);
}
