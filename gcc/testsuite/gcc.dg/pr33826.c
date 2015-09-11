/* Regression test for PR middle-end/33826 */
/* Verify that recursive functions cannot be pure or const.  */

/* { dg-do compile } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O1 -fdump-tree-local-pure-const1 -fdump-ipa-pure-const" } */

int recurse1 (int);
int recurse2b (int);
int norecurse1b (int);

int recurese1 (int i)
{
  return recurse1 (i+1);
}

int recurse2a (int i)
{
  return recurse2b (i+1);
}

int recurse2b (int i)
{
  return recurse2a (i+1);
}

int norecurse1a (int i)
{
  return norecurse1b (i+1);
}

int norecurse1b (int i)
{
  return i+1;
}

/* { dg-final { scan-tree-dump "found to be const: norecurse1a" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump "found to be const: norecurse1b" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "found to be pure: recurse1" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "found to be pure: recurse2a" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "found to be pure: recurse2b" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "found to be const: recurse1" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "found to be const: recurse2a" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "found to be const: recurse2b" "local-pure-const1" } } */
/* { dg-final { scan-ipa-dump-not "found to be pure: recurse1" "pure-const" } } */
/* { dg-final { scan-ipa-dump-not "found to be pure: recurse2a" "pure-const" } } */
/* { dg-final { scan-ipa-dump-not "found to be pure: recurse2b" "pure-const" } } */
/* { dg-final { scan-ipa-dump-not "found to be const: recurse1" "pure-const" } } */
/* { dg-final { scan-ipa-dump-not "found to be const: recurse2a" "pure-const" } } */
/* { dg-final { scan-ipa-dump-not "found to be const: recurse2b" "pure-const" } } */
