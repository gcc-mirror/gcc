/* PR tree-optimization/66757 */
/* Testcase by Zhendong Su <su@cs.ucdavis.edu> */

int a, b;

int
main (void)
{
  unsigned int t = (unsigned char) (~b); 

  if ((t ^ 1) / 255)
    __builtin_abort (); 

  return 0;
}
