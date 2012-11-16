/* { dg-do compile } */
/* { dg-options "-mavx -mvzeroupper" { target { i?86-*-* x86_64-*-* } } } */

short retframe_short (void *rframe)
{
  __builtin_return (rframe);
}
