/* { dg-do run } */
/* { dg-options "-O2" } */

typedef void (*fun) (void);

void __attribute__ ((noipa))
f (fun x1)
{
  register fun x2 asm ("x16");
  int arr[5000];
  int *volatile ptr = arr;
  asm ("mov %0, %1" : "=r" (x2) : "r" (x1));
  x2 ();
}

void g (void) {}

int
main (void)
{
  f (g);
}
