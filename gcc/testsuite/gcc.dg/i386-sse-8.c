/* PR target/14343 */
/* Origin: <Pawe Sikora <pluto@ds14.agh.edu.pl> */

/* The xstormy16 doesn't support V2DI.  */
/* { dg-do compile { xfail xstormy16-*-* } } */
/* { dg-options "" } */
/* { dg-options "-march=pentium3" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

int main() 
{ 
  typedef long long int v __attribute__ ((vector_size (16))); 
  v a, b; 
  a = b; 
  return 0; 
}
