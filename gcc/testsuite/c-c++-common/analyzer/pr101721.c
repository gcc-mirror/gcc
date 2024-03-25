/* PR analyzer/101721 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

void
foo ()
{
  __builtin_ia32_pause ();
}
