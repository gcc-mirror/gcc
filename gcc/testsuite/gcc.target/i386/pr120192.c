/* PR target/120192 */
/* { dg-do compile { target x86_64-*-mingw* } } */
/* { dg-options "-O0 -mavx" } */

typedef char a;
void b() {
  typedef a c __attribute__((vector_size(32), aligned));
  c d = {};
}
