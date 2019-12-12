/* PR ipa/78121 */
/* { dg-do compile } */
/* { dg-options "-ansi -O2 -fdump-ipa-cp-details" } */

void fn2 (unsigned char c);
int a;
static void fn1(c) unsigned char c;
{
    if (a)
          fn2 (c);
      fn1('#');
}

void fn3() { fn1 (267); }

/* { dg-final { scan-ipa-dump "Setting value range of param 0 \\(now 0\\) \\\[11, 35\\\]" "cp" } } */
