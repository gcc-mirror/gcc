/* Keep read and write attributes separate for a strub watermark update.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstrub=strict -fnon-call-exceptions" } */
/* { dg-additional-options "-fdelete-null-pointer-checks -fdump-rtl-expand" } */
/* { dg-require-effective-target strub } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

volatile int sink;

__attribute__ ((__strub__ ("internal"), noinline))
int
scrubbed (int value)
{
  sink = value;
  return sink;
}

/* The watermark load is nontrapping.  The conditional store can trap.  */
/* { dg-final { scan-rtl-dump {\(mem/f/c:[^\n]*watermark_ptr} "expand" } } */
/* { dg-final { scan-rtl-dump {\(mem/f:[^\n]*watermark_ptr} "expand" } } */
