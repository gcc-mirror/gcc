/* { dg-do run { target i?86-*-linux* } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

extern void abort (void);
extern void exit (int);

struct s {
  struct { int a; } a;
  int b;
  struct { struct { int a; } a; struct t { struct t *a, *b; } b; } c;
};

int bar(int (*fn)(void *), void *arg, unsigned long flags)
{
  return 0;
}

int baz(void *x)
{
  return 0;
}

void do_check (struct s *) asm ("do_check") __attribute__((regparm(1)));

void do_check(struct s *x)
{
  if (x->a.a || x->b || x->c.a.a)
    abort();
  if (x->c.b.a != &x->c.b || x->c.b.b != &x->c.b)
    abort();
}

#define NT "\n\t"

asm ("\n"
"___checkme:"
NT	"pushl %eax; pushl %ebx; pushl %ecx; pushl %edx; pushl %esi; pushl %edi"

NT	"pushl $0; pushl $0; pushl $0; pushl $0; pushl $0"
NT	"pushl $0; pushl $0; pushl $0; pushl $0; pushl $0"

NT	"movl %ecx, %eax"
NT	"call do_check"

NT	"popl %eax; popl %eax; popl %eax; popl %eax; popl %eax"
NT	"popl %eax; popl %eax; popl %eax; popl %eax; popl %eax"

NT	"popl %edi; popl %esi; popl %edx; popl %ecx; popl %ebx;	popl %eax"
NT	"ret"
);

extern inline void do_asm(struct s * x)
{
  asm volatile("call ___checkme" : : "c" (x) : "memory");
}

int foo(void)
{
  struct s x = { { 0 }, 0, { { 0 }, { &x.c.b, &x.c.b } } };
  bar(baz, &x, 1);
  do_asm(&x);
  bar(baz, &x, 1);
  do_asm(&x);
  return 0;
}

int main()
{
  foo();
  exit(0);
}
