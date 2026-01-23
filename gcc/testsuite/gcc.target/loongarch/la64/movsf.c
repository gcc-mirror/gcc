/* { dg-do compile } */
/* { dg-options "-mabi=lp64s" } */

float
__powisf2 (float x, int m)                                                      
{                                                                                
  unsigned int n = m < 0 ? -(unsigned int) m : (unsigned int) m;                 
  float y = n % 2 ? x : 1;                                                      
  while (n >>= 1)                                                                
    {                                                                            
      x = x * x;                                                                 
      if (n % 2)                                                                 
	y = y * x;                                                                      
    }                                                                            
  return m < 0 ? 1/y : y;                                                        
}   
