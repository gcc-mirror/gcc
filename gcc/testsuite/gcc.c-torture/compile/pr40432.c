/* Test that SRA produces valid gimple when handling both type punning by means
   of VCE and creating an access to a union.  */

union U {
  struct something *sth;
  void *nothing;
};

void
foo (union U *target, void *p)
{
  union U u;

  u.nothing = p;
  *target = u;
  return;
}
