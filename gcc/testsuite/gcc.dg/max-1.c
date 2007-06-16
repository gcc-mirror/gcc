/* PR middle-end/18548 */
/* Test case reduced by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do run } */
/* { dg-options "-O1 " } */
/* Option -fno-tree-lrs removed By Andrew MacLeod since it is no longer 
   supported in the compiler beginning with GCC 4.3.  */
/* m32c has varying sized pointers */
/* { dg-skip-if "" { "m32c-*-*" } { "*" } { "-mcpu=m32c" "-mcpu=m32cm" } } */

extern void abort (void);

long fff[10];

void f(long a, long b)
{
  long crcc = b;
  long d = *((long*)(a+1));
  int i;

  a = d >= b? d:b;


  for(i=0;i<10;i++)
   fff[i] = a;
}

/* The variable a cannot be a local variable as we get better aliasing
   now and decide that the store to a is dead.  The better aliasing comes
   from better representation of pointer arithmetic. */
long a = 10;
int main(void)
{
  int i;
  f((long)(&a)-1,0);
  for(i = 0;i<10;i++)
   if (fff[i]!=10)
    abort ();
  return 0;
}

