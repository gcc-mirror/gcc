/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized --param sra-max-structure-size=32" } */

/* Tests for SRA. */

typedef struct teststruct
{
  double d;
  char f1;
} teststruct;

void
copystruct1 (teststruct param)
{
  teststruct local;
  param.f1 = 0;
  local = param;
  if (local.f1 != 0)
    link_error ();
}

void
copystruct11 (teststruct *param)
{
  teststruct local;
  param->f1 = 0;
  local = *param;
  if (local.f1 != 0)
    link_error ();
}

void
copystruct111 (teststruct param)
{
  teststruct *local = &param;
  param.f1 = 0;
  if (local->f1 != 0)
    link_error ();
}

teststruct globf;
void
copystruct1111 (void)
{
  teststruct local;
  globf.f1 = 0;
  local = globf;
  if (local.f1 != 0)
    link_error ();
}

void
copystruct11111 (void)
{
  teststruct *local = &globf;
  globf.f1 = 0;
  if (local->f1 != 0)
    link_error ();
}

void
copystruct111111 (teststruct param)
{
  static teststruct local;
  param.f1 = 0;
  local = param;
  if (local.f1 != 0)
    link_error ();
}

/* There should be no referenc to link_error. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
