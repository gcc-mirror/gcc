/* { dg-do run } */
/* { dg-skip-if "No undefined weak" { hppa*-*-hpux* } } */
/* { dg-require-weak "" } */
/* { dg-additional-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } } */
/* { dg-additional-options "-Wl,-flat_namespace" { target *-*-darwin[89]* } } */

void __attribute__((noinline,noclone))
check (int i)
{
  if (i == 0)
    __builtin_exit (0);
}

int i;
extern int x __attribute__((weak));

int main(int argc, char **argv)
{
  if (argc)
    {
      check (i);
      return x;
    }
  else
    {
      check (i);
      return x-1;
    }
  return 0;
}
