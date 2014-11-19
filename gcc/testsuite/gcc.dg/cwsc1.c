/* { dg-do run } */
/* { dg-options "-O" } */

#if defined(__x86_64__)
# define CHAIN	"%r10"
#elif defined(__i386__)
# define CHAIN  "%ecx"
#elif defined(__aarch64__)
# define CHAIN  "x18"
#elif defined(__alpha__)
# define CHAIN  "$1"
#elif defined(__arm__)
# define CHAIN  "ip"
#elif defined(__powerpc__)
# define CHAIN  "11"
#elif defined(__s390__)
# define CHAIN  "%r0"
#elif defined(__sparc__)
# ifdef __arch64__
#  define CHAIN "%g5"
# else
#  define CHAIN "%g2"
# endif
#endif

#ifdef CHAIN
void *__attribute__((noinline, noclone)) foo(void)
{
  register void *chain __asm__(CHAIN);
  return chain;
}

void * (*ptr)(void) = foo;
extern void abort(void);

int main()
{
  char c;
  void *x = __builtin_call_with_static_chain(ptr(), &c);
  if (x != &c)
    abort();
  return 0;
}
#else
int main() { return 0; }
#endif
