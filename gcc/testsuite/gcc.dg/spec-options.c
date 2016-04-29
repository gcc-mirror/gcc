/* Check that -mfoo is accepted if defined in a user spec
   and that it is not passed on the command line.  */
/* Must be processed in EXTRA_SPECS to run.  */
/* { dg-do compile } */
/* { dg-do run { target sh*-*-* } } */
/* { dg-options "-B${srcdir}/gcc.dg --specs=foo.specs -tfoo" } */

extern void abort(void);

int main(void)
{
#ifdef FOO
  return 0;
#else
  abort();
#endif
}
