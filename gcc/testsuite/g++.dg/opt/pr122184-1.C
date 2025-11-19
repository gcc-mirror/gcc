// PR tree-optimization/122184
// { dg-do compile }
// { dg-require-stack-check "generic" }
// { dg-options "-O2 -fstack-check=generic" }

void
foo ()
{
  goto fail;
  char var[41];
fail:;
}
