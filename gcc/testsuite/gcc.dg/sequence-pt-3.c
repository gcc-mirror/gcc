/* More sequence point warning tests  */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

void bar(int i, int j)
{
  return;
}

void foo (int i)
{
   int a = i-i++; (void)a; /* { dg-warning "undefined" "sequence point warning" } */

   bar (i--, i++); /* { dg-warning "undefined" "sequence point warning" } */
}
