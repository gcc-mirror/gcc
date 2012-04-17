/* PR middle-end/18548 */
/* Test case reduced by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do run } */
/* { dg-options "-O1 " } */
/* Option -fno-tree-lrs removed By Andrew MacLeod since it is no longer 
   supported in the compiler beginning with GCC 4.3.  */
/* m32c has varying sized pointers */
/* { dg-skip-if "" { "m32c-*-*" } { "*" } { "-mcpu=m32c" "-mcpu=m32cm" } } */

__extension__ typedef __INTPTR_TYPE__ intptr_t;

extern void abort (void);

intptr_t fff[10];

void f(intptr_t a, intptr_t b)
{
  intptr_t crcc = b;
  intptr_t d = *((intptr_t*)(a+1));
  int i;

  a = d >= b? d:b;


  for(i=0;i<10;i++)
   fff[i] = a;
}

/* The variable a cannot be a local variable as we get better aliasing
   now and decide that the store to a is dead.  The better aliasing comes
   from better representation of pointer arithmetic. */
intptr_t a = 10;
int main(void)
{
  int i;
  f((intptr_t)(&a)-1,0);
  for(i = 0;i<10;i++)
   if (fff[i]!=10)
    abort ();
  return 0;
}

