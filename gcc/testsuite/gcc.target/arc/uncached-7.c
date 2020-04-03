/* { dg-options "-O1" } */
/* { dg-do compile } */

volatile __attribute__((uncached)) int s[20];

void s_acc(void)
{
    s[10] = 15;
}

/* { dg-final { scan-assembler-times "st\.di" 1 } } */
