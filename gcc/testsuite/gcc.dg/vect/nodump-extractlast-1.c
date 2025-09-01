/* Check for a bung in the treatment of LOOP_VINFO_PARTIAL_LOAD_STORE_BIAS when
   using VEC_EXTRACT.  */
/* { dg-require-effective-target vect_int } */

char c = 4;

__attribute__((noipa))
int
foo ()
{
  for ( ; c > 3; c -= 3);
  return c - 1;
}

int
main ()
{
  if (foo() != 0)
    __builtin_abort();
  return 0;
}
