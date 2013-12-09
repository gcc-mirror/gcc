/* { dg-do compile } */
/* { dg-options "-mips16" } */
extern void abort();

int i_0, i_1, i_2, i_3, i_4, i_5, i_6, i_7, i_8, i_9;
int j_0, j_1, j_2, j_3, j_4, j_5, j_6, j_7, j_8, j_9;

int main()
{
  register int *x1 = &i_1; 
  register int *x2 = &i_2; 
  register int *x3 = &i_3; 
  register int *x4 = &i_4; 
  register int *x5 = &i_5; 
  register int *x6 = &i_6;
  register int *x7 = &i_7; 
  register int *x8 = &i_8; 
  register int *x9 = &i_9;
  
  register int *y0 = &j_0; 
  register int *y1 = &j_1; 
  register int *y2 = &i_2; 
  register int *y3 = &j_3; 
  register int *y4 = &j_4; 
  register int *y5 = &j_5; 
  register int *y6 = &j_6; 
  register int *y7 = &j_7; 
  register int *y8 = &j_8; 
  register int *y9 = &j_9;

  asm volatile ("" : "=r" (x2) : "0" (x2)); 
  asm volatile ("" : "=r" (x3) : "0" (x3)); 
  asm volatile ("" : "=r" (x4) : "0" (x4)); 
  asm volatile ("" : "=r" (x5) : "0" (x5)); 
  asm volatile ("" : "=r" (x6) : "0" (x6)); 
  asm volatile ("" : "=r" (x7) : "0" (x7)); 
  asm volatile ("" : "=r" (x8) : "0" (x8)); 
  asm volatile ("" : "=r" (x9) : "0" (x9));

  asm volatile ("" : "=r" (y0) : "0" (y0)); 
  asm volatile ("" : "=r" (y1) : "0" (y1)); 
  asm volatile ("" : "=r" (y2) : "0" (y2)); 
  asm volatile ("" : "=r" (y3) : "0" (y3)); 
  asm volatile ("" : "=r" (y4) : "0" (y4));
  asm volatile ("" : "=r" (y5) : "0" (y5)); 
  asm volatile ("" : "=r" (y6) : "0" (y6)); 
  asm volatile ("" : "=r" (y7) : "0" (y7)); 
  asm volatile ("" : "=r" (y8) : "0" (y8)); 
  asm volatile ("" : "=r" (y9) : "0" (y9));

  asm volatile ("" : "=r" (x1) : "0" (x1)); 
  asm volatile ("" : "=r" (x2) : "0" (x2)); 
  asm volatile ("" : "=r" (x3) : "0" (x3)); 
  asm volatile ("" : "=r" (x4) : "0" (x4)); 
  asm volatile ("" : "=r" (x5) : "0" (x5)); 
  asm volatile ("" : "=r" (x6) : "0" (x6)); 
  asm volatile ("" : "=r" (x7) : "0" (x7)); 
  asm volatile ("" : "=r" (x8) : "0" (x8)); 
  asm volatile ("" : "=r" (x9) : "0" (x9));

  asm volatile ("" : "=r" (y0) : "0" (y0)); 
  asm volatile ("" : "=r" (y1) : "0" (y1)); 
  asm volatile ("" : "=r" (y2) : "0" (y2)); 
  asm volatile ("" : "=r" (y3) : "0" (y3)); 
  asm volatile ("" : "=r" (y4) : "0" (y4)); 
  asm volatile ("" : "=r" (y5) : "0" (y5)); 
  asm volatile ("" : "=r" (y6) : "0" (y6)); 
  asm volatile ("" : "=r" (y7) : "0" (y7)); 
  asm volatile ("" : "=r" (y8) : "0" (y8)); 
  asm volatile ("" : "=r" (y9) : "0" (y9));

  if (y0 != &j_0) abort (); 
  if (y1 != &j_1) abort (); 
  if (y2 != &j_2) abort (); 
  if (y3 != &j_3) abort (); 
  if (y4 != &j_4) abort (); 
  if (y5 != &j_5) abort (); 
  if (y6 != &j_6) abort (); 
  if (y7 != &j_7) abort (); 
  if (y8 != &j_8) abort (); 
  if (y9 != &j_9) abort ();
  return 0;
}
