/* { dg-do link } */
/* { dg-options "-O2" } */

/* PR middle-end/18628 exposed a problem in which cse folded a load
   from a jump table into the label that was the target of the branch.
   Unfortunately, the indirect jump was moved to a different basic
   block, and the LABEL_REF copied to the register wasn't enough to
   keep the cfg from optimizing the otherwise-unused label away.  So
   we ended up with a dangling reference to the label.  */

int i;

int main()
{
  for (;;)
  {
    switch (i)
    {
      case 0:
      case 1:
        return 1;

      case 2:
      case 3:
        return 0;

      case 5:
        --i;
    }
  }
}
