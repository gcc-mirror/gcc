/* PR17544 Incorrect -Wunreachable-code warning
   Origin: Giovanni Bajo

   In C99 we append a "return 0;" when finishing a function, but it was
   not given a source location.  The gimplifier thinks a return statement
   needs a locus so it would add one, making the compiler generated code
   visible to the unreachable code warning.  */

/* { dg-do compile } */
/* { dg-options "-std=c99 -O -Wunreachable-code" } */

int main (void)    // 1
{                  // 2
  return 0;        // 3
}                  // 4
