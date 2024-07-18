/* { dg-do compile } */
/* Annotate no return functions with a trailing 'trap'.  */

extern void abort ();

int main (int argc, char **argv)
{
  if (argc > 2)
    abort ();
  return 0;
}

/* { dg-final { scan-assembler "call abort;\[\r\n\t \]+trap;" } } */
