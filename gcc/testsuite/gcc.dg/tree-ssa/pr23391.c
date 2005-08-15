/* { dg-do compile } */

void 
foo (int N) 
{ 
  int C; 
  double R; 
 
  R = 0.0; 
  do 
    { 
      R += 0.001; 
      C = (int) (R * N); 
      if (-R * N <= R * N) 
        { 
          C++; 
        } 
    } 
  while (C < 0); 
 
  return; 
} 
