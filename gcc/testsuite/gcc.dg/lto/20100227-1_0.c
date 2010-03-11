/* { dg-lto-do link } */
/* { dg-extra-ld-options "-w" } */

/* Make sure we do not ICE on the invalid re-declaration of s.  */

extern void f(void);
const char *s = "Hello, world!";

int main(void)
{
  f();
  return 0;
}

