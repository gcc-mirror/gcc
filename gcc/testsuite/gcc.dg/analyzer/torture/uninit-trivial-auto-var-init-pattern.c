/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-additional-options "-ftrivial-auto-var-init=pattern" } */

int test_1 (void)
{
  int i; /* { dg-message "region created on stack here" } */
  return i; /* { dg-warning "use of uninitialized value 'i.*'" } */
  /* FIXME: the LTO build sometimes shows SSA names here
     (PR analyzer/94976).  */
}
