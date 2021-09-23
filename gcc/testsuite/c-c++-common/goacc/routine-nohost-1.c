/* Test OpenACC 'routine' with 'nohost' clause, valid use.  */

/* { dg-additional-options "-fdump-tree-oaccloops" } */

#pragma acc routine nohost
int THREE(void)
{
  return 3;
}

#pragma acc routine (THREE) nohost

#pragma acc routine nohost
extern int THREE(void);

/* { dg-final { scan-tree-dump-times {(?n)^OpenACC routine '[^']*THREE[^']*' has 'nohost' clause\.$} 1 oaccloops } } */


#pragma acc routine nohost
extern void NOTHING(void);

#pragma acc routine (NOTHING) nohost

void NOTHING(void)
{
}

#pragma acc routine nohost
extern void NOTHING(void);

#pragma acc routine (NOTHING) nohost

/* { dg-final { scan-tree-dump-times {(?n)^OpenACC routine '[^']*NOTHING[^']*' has 'nohost' clause\.$} 1 oaccloops } } */


extern float ADD(float, float);

#pragma acc routine (ADD) nohost

float ADD(float x, float y)
{
  return x + y;
}

#pragma acc routine nohost
extern float ADD(float, float);

#pragma acc routine (ADD) nohost

/* { dg-final { scan-tree-dump-times {(?n)^OpenACC routine '[^']*ADD[^']*' has 'nohost' clause\.$} 1 oaccloops } } */
