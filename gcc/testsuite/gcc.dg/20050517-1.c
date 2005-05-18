/* Tree PRE is going to transform this so that it doesn't call cos on the 
   d = 0 path, and in doing so, it needs to regenerate the cos call.
   This was ICE'ing due to an overly strict check on what it knew how
   to regenerate.   */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */
double cos(double);
double f(double d, double i, int j) { if (j == 1) d = 0; return d * cos(i); }
