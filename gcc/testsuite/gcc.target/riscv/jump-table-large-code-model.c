/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -mcmodel=large" } */

int foo(int x, int y)
{
  switch(x){
  case 0:
    return 123 + y;
  case 1:
    return 456 + y;
  case 2:
    return 789 - y;
  case 3:
    return 12 * y;
  case 4:
    return 13 % y;
  case 5:
    return 11 *y;
  }
  return 0;
}


/* { dg-final { scan-assembler-not "\.section	\.rodata" } } */
