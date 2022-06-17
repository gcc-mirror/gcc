// { dg-options "-fsanitize=address -O3 -fdump-tree-optimized" }
// { dg-do compile }

import gcc.attributes;

@no_sanitize("address")
__gshared int globalvar1; // { dg-warning "attribute ignored" }

pragma(inline, true)
@no_sanitize("address")
void test_no_address()
{
    counter++;
}

pragma(inline, true)
void test_sanitize()()
{
    counter++;
}

void func1()
{
  counter++;
  test_no_address();
  test_sanitize();
}

private int counter;

// { dg-final { scan-tree-dump-times "Function test_no_address" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "Function test_sanitize" 0 "optimized" } }
