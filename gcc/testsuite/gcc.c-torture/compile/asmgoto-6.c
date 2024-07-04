/* { dg-do compile { target asm_goto_with_outputs } } */
/* PR middle-end/110420 */
/* PR middle-end/103979 */
/* PR middle-end/98619 */
/* Test that the middle-end does not remove the asm goto
   with an output. */

static int t;
void g(void);

void f(void)
{
  int  __gu_val;
  asm goto("#my asm "
     : "=&r"(__gu_val)
     :
     :
     : Efault);
  t = __gu_val;
  g();
Efault:
}

/* Make sure "my asm " is still in the assembly. */
/* { dg-final { scan-assembler "my asm " } } */
