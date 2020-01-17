/* PR inline-asm/93027 */
/* { dg-do compile  { target x86_64-*-* } } */
/* { dg-options "-O0" } */

int main (void) {
  int f = 0, w;

  asm volatile(
    ""
    : "+m&l"(f)
    : "0a"(&w)
  );
  return 0;
}
