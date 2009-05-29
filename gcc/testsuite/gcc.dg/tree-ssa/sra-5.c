/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Tests for SRA of unions. */


typedef union testunion
{
  double d;
  char f1;
} testunion;

void
copyunion1 (testunion param)
{
  testunion local;
  param.f1 = 0;
  local = param;
  if (local.f1 != 0)
    link_error ();
}

void
copyunion11 (testunion *param)
{
  testunion local;
  param->f1 = 0;
  local = *param;
  if (local.f1 != 0)
    link_error ();
}

void
copyunion111 (testunion param)
{
  testunion *local = &param;
  param.f1 = 0;
  if (local->f1 != 0)
    link_error ();
}

testunion globuf;
void
copyunion1111 (void)
{
  testunion local;
  globuf.f1 = 0;
  local = globuf;
  if (local.f1 != 0)
    link_error ();
}

void
copyunion11111 (void)
{
  testunion *local = &globuf;
  globuf.f1 = 0;
  if (local->f1 != 0)
    link_error ();
}

void
copyunion111111 (testunion param)
{
  static testunion local;
  param.f1 = 0;
  local = param;
  if (local.f1 != 0)
    link_error ();
}

/* There should be no reference to link_error. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
