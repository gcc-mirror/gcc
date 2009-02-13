/* PR target/38056.  Do sibcall optimization across object file
   boundery when -mconstant-gp is used.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mconstant-gp" } */
/* { dg-final { scan-assembler-not "br.call.*bar" } } */

int bar(int x);

int foo(int x)
{
  return (bar(x + 1));
}
