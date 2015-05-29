/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

void link_error (void);

class base
{
};

class teststruct: public base
{
public:
  double d;
  char f1;
};

void
copystruct1 (teststruct param)
{
  teststruct local;
  param.f1 = 0;
  local = param;
  if (local.f1 != 0)
    link_error ();
}

/* There should be no reference to link_error. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
