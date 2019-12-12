/* PR tree-optimization/89536 */
/* Testcase by Zhendong Su <su@cs.ucdavis.edu> */

int a = 1;

int main (void)
{
  a = ~(a && 1); 
  if (a < -1)
    a = ~a;
  
  if (!a)
    __builtin_abort ();

  return 0;
}
