/* { dg-do compile } */

/* Test clobbers.
   See asm-hard-reg-error-{2,3}.c for tests involving register pairs.  */

int
test (void)
{
  int x, y;
  __asm__ ("" : "={0}" (x), "={1}" (y) : : "1"); /* { dg-error "hard register constraint for output 1 conflicts with 'asm' clobber list" } */
  __asm__ ("" : "={0}" (x) : "{0}" (y), "{1}" (y) : "1"); /* { dg-error "hard register constraint for input 1 conflicts with 'asm' clobber list" } */
  return x + y;
}
