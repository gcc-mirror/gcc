/* { dg-do compile } */

int my_errno;

__attribute__((noipa)) double
my_log (double d)
{
  my_errno = 33;
  return 0.0;
}
