/* PR target/98100 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -fvar-tracking-assignments -g0" } */

__attribute__((target_clones("default","avx2"))) void
foo ()
{
  __attribute__((__vector_size__(8 * sizeof(int)))) int b = {};
}
