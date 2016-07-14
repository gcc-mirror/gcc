/* { dg-options "-O2 -fdump-rtl-expand-all" } */
int g;

__attribute__((noinline)) void foo (int  n)
{
  switch (n)
    {
    case 99:
      g += 2; break;
    case 1:
      g++; break;
    case 100:
      g += 1; break;
    case 4:
      g += 3; break;
    case 5:
      g += 4; break;
    case 6:
      g += 5; break;
    case 7:
      g += 6; break;
    case 8:
      g += 7; break;
    case 9:
      g += 8; break;
    default:
      g += 8; break;
   }
}

int main ()
{
 int i;
 for (i = 0; i < 10000; i++)
   foo ((i * i) % 5);
 return 0;
}
/* autofdo cannot do that precise execution numbers: */
/* { dg-final-use-not-autofdo { scan-rtl-dump-times ";; basic block\[^\\n\]*count 4000" 2 "expand"} } */
/* { dg-final-use-not-autofdo { scan-rtl-dump-times ";; basic block\[^\\n\]*count 2000" 1 "expand"} } */
