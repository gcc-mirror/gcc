/* ICE due to invalid GIMPLE created during strlen simplification.  */

extern unsigned long strlen (__const char *__s);
extern void bar ();
extern int cols;

void foo (void)
{
  char s[cols + 2];
  if (strlen (s) > 0)
    bar ();
}
