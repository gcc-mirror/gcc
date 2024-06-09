/* { dg-require-effective-target untyped_assembly } */
static int kind_varread(char *str)
{
  if (0 == __builtin_memcmp("%_#",               str, 3))  return 2;
  /* NOTREACHED */
}

