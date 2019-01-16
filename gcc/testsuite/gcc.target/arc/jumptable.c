/* { dg-do compile } */
/* { dg-skip-if "" { arc700 || arc6xx } } */
/* { dg-options "-O2 -mbranch-index -mcode-density" { target { arcem || archs } } } */

extern void max( int,int);

int switchCase(int value, int b)
{
  switch(value){
  case 100:
    value = b * value;
    break;
  case 101:
    value = b << value;
    break;
  case 102:
    value = b / value;
    break;
  case 103:
    value = b >> value;
    break;
  case 104:
    value = b + value;
    break;
  case 105:
    value = b - value;
    break;
  }
  max(value, b);
  return 0;
}

/* { dg-final { scan-assembler-times "bih" 1 } } */
/* { dg-final { scan-assembler-times "b_s" 8 } } */
