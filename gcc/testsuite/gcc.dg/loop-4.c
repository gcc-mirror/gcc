/* PR optimization/11841 */
/* Originator: Andrey Panov <panov@canopus.iacp.dvo.ru> */
/* Reduced testcase by Volker Reichelt <reichelt@igpm.rwth-aachen.de> */

/* Verify that the (old) loop unroller doesn't wrongly mark a pseudo
   referenced in a note as local.  */

/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops" } */

int *a;

int main()
{
  double d[6];
  int i, j;

  for (i=0; i<4; ++i)
    for (j=0; j<3; ++j)
      d[i+j] = 0;

  a = &i;

  return 0;
}
