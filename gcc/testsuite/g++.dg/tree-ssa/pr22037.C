/* { dg-do compile } */
/* { dg-options "-O2" } */

extern double sqrt (double) throw (); 
 
void foo(double& d, int n) 
{ 
  double e=0; 
  for(int i=0; i<n; i++); 
  for(int i=0; i<n; i++) e=1; 
  d = sqrt(e); 
 
  for(int i=0; i<n; i++); 
}
