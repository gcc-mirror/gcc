/* { dg-do compile } */

__attribute__((no_sanitize("hwaddress"))) int
f (int *p, int *q)
{
  *p = 42;
  return *q;
}

/* Only have one instance of __hwasan, it is __hwasan_init (the module
 * constructor) there is no instrumentation in the function.  */
/* { dg-final { scan-assembler-times "__hwasan" 1 } } */
