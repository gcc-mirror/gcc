/* { dg-do compile } */
/* { dg-options "-O2 -fsched2-use-superblocks -g -w" } */
/* { dg-options "-O2 -fsched2-use-superblocks -g -mbackchain" { target s390*-*-* } } */
/* { dg-require-effective-target scheduling } */

void *
foo (int offset)
{
  switch (offset)
    {
    case 0:
      return __builtin_return_address (0);
    case 1:
      return __builtin_return_address (1);
    case 2:
      return __builtin_return_address (2);
    case 3:
      return __builtin_return_address (3);
    case 4:
      return __builtin_return_address (4);
    }
  return 0;
}
