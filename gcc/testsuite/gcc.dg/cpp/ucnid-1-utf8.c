/* { dg-do run } */
/* { dg-options "-std=c99 -g3" } */
void abort (void);

#define À 1
#define Á 2
#define Â 3
#define whÿ 4
#define aÄbсδe 5

int main (void)
{
  
  if (À != 1)
    abort ();
  if (Á != 2)
    abort ();
  if (Â != 3)
    abort ();
  if (whÿ != 4)
    abort ();
  if (aÄbсδe != 5)
    abort ();
  
  return 0;
}
