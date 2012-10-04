/* Make sure that by reference and by value aggregate jump functions do not get
   mixed up.
   ??? This testcase is invalid C and can only pass on specific platforms.  */
/* { dg-lto-do run } */
/* { dg-skip-if "" { { sparc*-*-* } && ilp32 } { "*" } { "" } } */
/* { dg-lto-options { {-O3 -fno-early-inlining -flto}} } */

extern void abort (void);

struct S
{
  int i;
  void (*f)(struct S *);
  int j;
};

struct E
{
  struct S *p;
};

struct S *gs;
int gr = 111;
char gc[1024];

static __attribute__ ((noinline, noclone)) struct S *
get_s (void)
{
  return (struct S *) &gc;
}

static void wrong_target (struct S *s)
{
  abort ();
}

void bar (struct S *s)
{
  s->f (s);
}

extern void foo (struct S *s);

int main (int argc, char **argv)
{
  struct S *s = get_s();
  gs = s;
  s->i = 5678;
  s->f = wrong_target;
  s->j = 1234;
  foo (s);

  return gr;
}
