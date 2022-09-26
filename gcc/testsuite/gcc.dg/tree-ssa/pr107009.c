// { dg-do compile }
// { dg-options "-O2 -fdump-tree-dom2-alias" }

typedef __SIZE_TYPE__ size_t;

void saxpy(size_t n)
{
  if (n == 0 || n % 8 != 0)
    __builtin_unreachable();

  extern void foobar (size_t n);
  foobar (n);
}

// { dg-final { scan-tree-dump "NONZERO.*fff8" "dom2" } }
