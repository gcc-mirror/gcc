// PR tree-optimization/127183
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fdump-tree-optimized" }

struct s1
{
    int f();
    int a;
};

int s1::f()
{
    try {
      this->a = 1;
    }catch(...)
    {
        __builtin_trap ();
    }
    return 0;
}

// An write access to this should be still considered as trapping
// and an throwable for non-call exceptions.

// { dg-final { scan-tree-dump "__builtin_trap " "optimized" } } */
