/* PR target/38056.  Do not do sibcall optimization across object file
   boundery when -mconstant-gp is not used.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "br.call.*bar" } } */

int bar(int x);

int foo(int x)
{
  return (bar(x + 1));
}
