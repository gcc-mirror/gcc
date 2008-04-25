/* PR middle-end/18548 */
/* Test case reduced by Andrew Pinski <pinskia@physics.uc.edu> */
/* { dg-do run } */
/* { dg-options "-O1 " } */
/* Option -fno-tree-lrs removed By Andrew MacLeod since it is no longer 
   supported in the compiler beginning with GCC 4.3.  */
/* m32c has varying sized pointers */
/* { dg-skip-if "" { "m32c-*-*" } { "*" } { "-mcpu=m32c" "-mcpu=m32cm" } } */

/* Kludge to make it signed. */
#define unsigned signed
__extension__ typedef __SIZE_TYPE__ ssize_t;
#undef unsigned

extern void abort (void);

ssize_t fff[10];

void f(ssize_t a, ssize_t b)
{
  ssize_t crcc = b;
  ssize_t d = *((ssize_t*)(a+1));
  int i;

  a = d >= b? d:b;


  for(i=0;i<10;i++)
   fff[i] = a;
}

/* The variable a cannot be a local variable as we get better aliasing
   now and decide that the store to a is dead.  The better aliasing comes
   from better representation of pointer arithmetic. */
ssize_t a = 10;
int main(void)
{
  int i;
  f((ssize_t)(&a)-1,0);
  for(i = 0;i<10;i++)
   if (fff[i]!=10)
    abort ();
  return 0;
}

