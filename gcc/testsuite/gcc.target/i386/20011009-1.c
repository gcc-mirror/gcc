/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);
extern void exit (int);

#ifdef __sun__
#define COMMENT "/"
#else
#define COMMENT "#"
#endif

int main ()
{
  int x;

  asm ("movl $26, %0 " COMMENT " 26 |-> reg \n\t"
       "movl $28, %0" : "=r" (x));
  if (x != 28)
    abort ();
  exit (0);
}
