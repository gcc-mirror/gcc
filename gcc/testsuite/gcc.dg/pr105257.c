/* PR target/105257 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fpic" { target fpic } } */

extern int sigsetjmp (void **, int);
void *buf[32];
void (*fn) (void);

const char *
bar (void)
{
  sigsetjmp (buf, 0);
  fn ();
  return "";
}
