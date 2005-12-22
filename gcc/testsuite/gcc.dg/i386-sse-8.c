/* PR target/14343 */
/* Origin: <Pawe Sikora <pluto@ds14.agh.edu.pl> */

/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-march=pentium3" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

int main() 
{ 
  typedef int v __attribute__ ((mode(V2DI))); 
  v a, b; 
  a = b; 
  return 0; 
}
