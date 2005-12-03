/* PR target/14313 */
/* Origin: <Pawe Sikora <pluto@ds14.agh.edu.pl> */

/* { dg-do compile } */
/* { dg-options "-march=pentium3" { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */

int main() 
{ 
  typedef int v __attribute__ ((mode(V2DI))); 
  v a, b; 
  a = b; 
  return 0; 
}
