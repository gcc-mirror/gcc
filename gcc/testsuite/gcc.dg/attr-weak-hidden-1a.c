/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */
void abort (void);
int __attribute__((weak, visibility("hidden"))) foo (void) { return 1; }
int
main (void)
{
  if (foo ())
    abort ();
  return 0;
}
