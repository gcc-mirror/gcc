/* PR target/14343 */
/* Origin: <Pawe Sikora <pluto@ds14.agh.edu.pl> */

/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-march=pentium3" } */

int main() 
{ 
  typedef long long int v __attribute__ ((vector_size (16))); 
  v a, b; 
  a = b; 
  return 0; 
}
