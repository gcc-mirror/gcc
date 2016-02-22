/* PR target/69888 */
/* { dg-do compile } */
/* { dg-options "-minline-all-stringops -mmemset-strategy=no_stringop:-1:noalign" } */
/* { dg-additional-options "-march=geode" { target ia32 } } */

void
foo (char *p)
{
  __builtin_memset (p, 0, 32);
}
