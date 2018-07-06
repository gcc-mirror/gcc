/* { dg-do compile } */
/* { dg-options "-O1 */


#define __aux() __attribute__((aux))

__aux() int *a_ptr;
extern __aux() int a_var;

/* Generates:
   mov r0, @a_var
   sr  10,[r0]
*/
void foo (void)
{
  a_var = 10;
}

/* Generates:
   mov r0, @a_ptr
   sr  a_var,[r0]
*/
void foo1 (void)
{
  a_ptr = &a_var;
}

/* Generates:
   lr  %r1,[a_ptr]
   sr  10,[%r1]
*/
void foo2 (void)
{
  *a_ptr = 10;
}

/* { dg-final { scan-assembler-times "sr" 3 } } */
/* { dg-final { scan-assembler-times "lr" 1 } } */
