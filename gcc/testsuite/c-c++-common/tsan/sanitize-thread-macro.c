/* Check that -fsanitize=thread options defines __SANITIZE_THREAD__ macros.  */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

int
main ()
{
#ifndef __SANITIZE_THREAD__
  bad construction
#endif
  return 0;
}
