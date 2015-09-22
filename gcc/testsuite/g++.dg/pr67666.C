// { dg-do compile }
// { dg-options "-O2 -fdump-tree-ealias-all" }

struct ps
{
  int *__restrict__ p;
};

void
f (struct ps &__restrict__ ps1)
{
  *(ps1.p) = 1;
}

// { dg-final { scan-tree-dump-times "clique 1 base 1" 1 "ealias" } }
// { dg-final { scan-tree-dump-times "clique 1 base 2" 1 "ealias" } }
// { dg-final { scan-tree-dump-times "(?n)clique .* base .*" 2 "ealias" } }
