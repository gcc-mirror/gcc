/* Test to make sure the mcaro info includes a start file command for the main source */
/* { dg-do compile */
/* { dg-options "-g3 -gdwarf-2 -dA -fverbose-asm" } */
/* { dg-final { scan-assembler "Start new file" } } */

#define ADD(x) (M + x)

int main (void)
{
#define N 28
#define M 42
   return ADD(N);
}
