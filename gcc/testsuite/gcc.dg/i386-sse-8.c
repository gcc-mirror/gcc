/* PR target/14313 */
/* Origin: <Pawe Sikora <pluto@ds14.agh.edu.pl> */

/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-march=pentium3" { target i?86-*-* } } */

int main() 
{ 
  typedef long long int v __attribute__ ((vector_size (16))); 
  v a, b; 
  a = b; 
  return 0; 
}
