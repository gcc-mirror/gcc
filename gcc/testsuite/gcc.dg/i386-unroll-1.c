/* PR optimization/8599 */
/* { dg-do run { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-mtune=k6 -O2 -funroll-loops" } */

extern void exit (int);

void *array[4];

int main ()
{
  int i;
 
  for (i = 0; i < 4; i++)
    array[i] = 0;

  exit (0);
}
