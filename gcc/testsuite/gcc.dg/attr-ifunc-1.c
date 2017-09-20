/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "" } */

typedef int F (void);

static int implementation (void)
{
  __builtin_printf ("'ere I am JH\n");
  return 0;
}

static F* resolver (void)
{
  return implementation;
}

extern int magic (void) __attribute__ ((ifunc ("resolver")));

int main ()
{
  return magic () != 0;
}
