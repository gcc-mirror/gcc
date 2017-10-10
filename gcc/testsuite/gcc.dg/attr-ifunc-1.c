/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "" } */

static int implementation (void)
{
  __builtin_printf ("'ere I am JH\n");
  return 0;
}

static __typeof__ (implementation)* resolver (void)
{
  return implementation;
}

extern int magic (void) __attribute__ ((ifunc ("resolver")));

int main ()
{
  return magic () != 0;
}
