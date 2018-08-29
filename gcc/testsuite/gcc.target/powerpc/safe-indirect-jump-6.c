/* { dg-do run } */
/* { dg-additional-options "-mno-speculate-indirect-jumps" } */
/* { dg-warning "'-mno-speculate-indirect-jumps' is deprecated" "" { target *-*-* } 0 } */

/* Test for deliberate misprediction of jump tables.  */

void __attribute__((noinline)) bar ()
{
}

int foo (int x)
{
  int a;
  
  switch (x)
    {
    default:
      a = -1;
      break;
    case 0:
      a = x * x + 3;
      break;
    case 1:
      a = x + 1;
      break;
    case 2:
      a = x + x;
      break;
    case 3:
      a = x << 3;
      break;
    case 4:
      a = x >> 1;
      break;
    case 5:
      a = x;
      break;
    case 6:
      a = 0;
      break;
    case 7:
      a = x * x + x;
      break;
    }

  bar();

  return a;
}

int main ()
{
  if (foo (0) != 3)
    __builtin_abort ();
  
  if (foo (1) != 2)
    __builtin_abort ();
  
  if (foo (2) != 4)
    __builtin_abort ();
  
  if (foo (3) != 24)
    __builtin_abort ();
  
  if (foo (4) != 2)
    __builtin_abort ();
  
  if (foo (5) != 5)
    __builtin_abort ();
  
  if (foo (6) != 0)
    __builtin_abort ();
  
  if (foo (7) != 56)
    __builtin_abort ();
  
  if (foo (8) != -1)
    __builtin_abort ();
  
  return 0;
}
