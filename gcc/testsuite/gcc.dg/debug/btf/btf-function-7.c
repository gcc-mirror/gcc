/* Test BTF for inlined functions.

   See PR/112656 - btf: function prototypes generated with name
   BTF_KIND_FUNC_PROTO must be anonymous.  */

/* { dg-do compile } */
/* { dg-options "-O2 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "BTF_KIND_FUNC_PROTO ''\\(\[0-9a-z\]*\\)'" 0 } } */

static int log_event(const char *event_name, void *dev_ptr)
{
  return 666;
}

int foo ()
{
  return log_event ("foobar", ((void *)0));
}
