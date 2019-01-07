/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate -std=c++11" } */

int a, b, c;

void
bar ()
{
  switch (a)
  {
    case 3: __builtin_puts("a"); break;
    [[unlikely]] case 42: __builtin_puts("e"); break;
    [[likely]] case 333: __builtin_puts("i"); break;
  } 
}

/* { dg-final { scan-tree-dump "default.*4.98%.*case 3.*4.98%.*case 42.*0.05%.*case 333.*90.00%" "profile_estimate"} } */
