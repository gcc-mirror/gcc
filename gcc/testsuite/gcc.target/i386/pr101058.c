/* PR target/101058 */
/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -mno-mmx" } */

short add90Hybrid_a_1;
short *add90Hybrid_b, *add90Hybrid_c, *add90Hybrid_d;
void add90Hybrid() {
  for (int i; i < 200; i += 2) {
    add90Hybrid_c[i] = add90Hybrid_b[i];
    add90Hybrid_d[i] = add90Hybrid_a_1 - add90Hybrid_b[i + 1];
  }
}
