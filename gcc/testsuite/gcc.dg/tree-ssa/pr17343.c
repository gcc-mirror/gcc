/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
void foo0(void);
void foo1(void);
void foo2(void);
void foo3(void);

void
foo (int a)
{
  switch (a)
    {
    case 10:
    case 11:
    case 12:
    case 13:
      goto ddd;
    case 14:
      foo1();
      break;
    case 15:
      foo2();
      break;
    case 16:
      foo3();
      break;
    default:
    ddd:
      foo0();
      break;
    }
}
/* There should be precisely two references to ddd.  One in the switch
   and one for the label, we used not to combine the case 10-13 into
   the default case.  */
/* { dg-final { scan-tree-dump-times "ddd" 1 "optimized"} } */
