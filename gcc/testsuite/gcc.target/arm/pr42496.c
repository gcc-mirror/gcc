/* { dg-options "-O2" }  */

void foo(int i)
{
    extern int j;

    if (i) {
         j = 10;
    }
    else {
          j = 20;
    }
}

/* { dg-final { scan-assembler-not "strne" } } */
/* { dg-final { scan-assembler-not "streq" } } */
