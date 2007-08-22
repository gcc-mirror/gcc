/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);

int main ()
{
  int x;

  asm ("movl $26, %0 # 26 |-> reg \n\t"
       "movl $28, %0" : "=r" (x));
  if (x != 28)
    abort ();
  exit (0);
}
