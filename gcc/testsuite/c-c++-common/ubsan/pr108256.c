/* PR sanitizer/108256 */
/* { dg-do run { target { lp64 || ilp32 } } } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

unsigned short
foo (unsigned short x, unsigned short y)
{
  return x * y;
}

unsigned short
bar (unsigned short x, unsigned short y)
{
  int r = x * y;
  return r;
}

int
main ()
{
  volatile unsigned short a = foo (0xffff, 0xffff);
  volatile unsigned short b = bar (0xfffe, 0xfffe);
  return 0;
}

/* { dg-output "signed integer overflow: 65535 \\\* 65535 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 65534 \\\* 65534 cannot be represented in type 'int'" } */
