/* { dg-do compile } */
/* { dg-options "-fjump-tables -mno-speculate-indirect-jumps" } */
/* { dg-warning "'-mno-speculate-indirect-jumps' is deprecated" "" { target *-*-* } 0 } */

/* Test for deliberate misprediction of jump tables.  */

void bar (void);

int foo (int x)
{
  int a;
  
  switch (x)
    {
    default:
      a = -1;
      break;
    case 0:
      a = x * x;
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

/* { dg-final { scan-assembler "crset" } } */
/* { dg-final { scan-assembler "beqctr-" } } */
/* { dg-final { scan-assembler {b \$} } } */
