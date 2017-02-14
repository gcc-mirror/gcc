// { dg-do compile }
// { dg-options "-Wall -fdump-tree-eh" }

extern "C"
void fork () // { dg-warning "conflicts with built-in declaration" }
__attribute__ ((__nothrow__));

void foo () throw ()
{
  fork ();
}

// { dg-final { scan-tree-dump-not "eh_dispatch" "eh" } }
// { dg-final { scan-tree-dump-not "resx" "eh" } }
