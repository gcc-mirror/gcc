/* { dg-do compile } */
/* { dg-options "-fexceptions" } */
/* PR28516: ICE generating ARM unwind directives for nested functions.  */
/* { dg-require-effective-target trampolines } */
/* { dg-require-effective-target exceptions } */

void ex(int (*)(void));
void foo(int i)
{
  int bar(void)
  {
    return i;
  }
  ex(bar);
}
