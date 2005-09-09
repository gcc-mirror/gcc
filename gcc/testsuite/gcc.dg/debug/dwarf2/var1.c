/* PR 23190 */
/* { dg-do compile }
/* { dg-options "-gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "xyzzy\[^\\n\\r\]+DW_AT_name" } } */

void f(void)
{
   static int xyzzy;
   xyzzy += 3;
}
