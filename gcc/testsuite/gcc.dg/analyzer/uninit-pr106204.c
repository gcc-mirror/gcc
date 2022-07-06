/* { dg-additional-options "-ftrivial-auto-var-init=zero" } */

int foo(unsigned *len);

/* Modified version of reproducer that does use "len" before init.  */

int test_2()
{
 unsigned len;
 int rc;

 rc = len; /* { dg-warning "use of uninitialized value 'len'" } */
 rc = foo(&len);
 if (!rc)
  rc = len;
 return rc;
}
