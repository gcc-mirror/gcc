/* PR target/128640 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f" } */

typedef float v8sf __attribute__((vector_size (32)));
typedef float v16sf __attribute__((vector_size (64)));
union u1 { v8sf f; };
union u2 { v16sf f; };

void bar (int, ...);

/* Unnamed 256-bit and 512-bit unions are passed on the stack, so no
   SSE register is used for the variadic arguments.  */
void
foo1 (union u1 *p)
{
  bar (1, *p);
}

void
foo2 (union u2 *p)
{
  bar (1, *p);
}

/* { dg-final { scan-assembler-times "xorl\[\\t \]+%eax, %eax" 2 } } */
/* { dg-final { scan-assembler-not "movl\[\\t \]+\\\$1, %eax" } } */
