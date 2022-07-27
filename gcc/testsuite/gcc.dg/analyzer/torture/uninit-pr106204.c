/* { dg-additional-options "-ftrivial-auto-var-init=zero" } */

int foo(unsigned *len);
int test_1()
{
 unsigned len; /* { dg-bogus "uninit" } */
 int rc;

 rc = foo(&len);
 if (!rc)
  rc = len;
 return rc;
}
