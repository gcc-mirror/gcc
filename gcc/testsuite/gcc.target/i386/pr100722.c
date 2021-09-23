/* PR target/100722 */
/* { dg-do compile } */
/* { dg-options "-O -msse2" } */

typedef char int8x4_t __attribute__((vector_size(4)));

void stack_callee (int8x4_t, int8x4_t, int8x4_t, int8x4_t,
		   int8x4_t, int8x4_t, int8x4_t);

int8x4_t stack_caller_x1;

void stack_caller (void)
{
  stack_callee (stack_caller_x1, stack_caller_x1, stack_caller_x1,
		stack_caller_x1, stack_caller_x1, stack_caller_x1,
		stack_caller_x1);
}
