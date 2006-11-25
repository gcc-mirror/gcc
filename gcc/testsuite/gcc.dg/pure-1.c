/* Regression test for PR middle-end/23584 */
/* Verify that dereferencing a volatile element in a struct causes
   the function not be pure.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-ipa-pure-const" } */

struct test_a { volatile int a; };

int func_a(struct test_a *a)
{
        return a->a;
}

/* { dg-final { scan-ipa-dump-not "found to be pure: func_a" "pure-const" } } */
/* { dg-final { cleanup-ipa-dump "pure-const" } } */
