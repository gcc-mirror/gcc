// { dg-do compile }
// { dg-options "-O3 -fdump-tree-optimized-raw" }

import gcc.attributes;

int glob1;
int easily_inlinable(int i) { glob1 = i; return 2; }

@optStrategy("none")
int call_easily_inlinable(int i)
{
    return easily_inlinable(i);
}

// { dg-final { scan-tree-dump "gimple_call <easily_inlinable" "optimized" } }

pragma(inline, true) int always_inline() { return 321; }

@optStrategy("none")
int call_always_inline()
{
    return always_inline();
}

// { dg-final { scan-tree-dump "gimple_call <always_inline" "optimized" } }

int glob2;
@optStrategy("none") void optnone_function(int i) { glob2 = i; }

void call_optnone()
{
    optnone_function(1);
}

// { dg-final { scan-tree-dump "gimple_call <optnone_function" "optimized" } }

@optStrategy("optsize")
void optsize_fn()
{
}

@optStrategy("minsize")
void minsize_fn()
{
}
