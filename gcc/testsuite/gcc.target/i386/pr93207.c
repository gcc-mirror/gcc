/* PR inline-asm/93207 */
/* { dg-do compile } */
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
