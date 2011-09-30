/* { dg-do compile } */
/* { dg-options "-O2" } */

long long foo (signed char * arg)
{
  long long temp_1;

  temp_1 = arg[256]; 
  return temp_1;
}
