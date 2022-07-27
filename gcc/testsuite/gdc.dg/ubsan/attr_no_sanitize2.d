// { dg-options "-fsanitize=undefined -O3 -fdump-tree-optimized" }
// { dg-do compile }

import gcc.attributes;

@no_sanitize("invalid_name")
void func1() { } // { dg-warning "attribute directive ignored" }

@no_sanitize("address")
@no_sanitize("thread")
@no_sanitize("address,thread")
@no_sanitize("address", "undefined")
@no_sanitize("undefined", "leak", "return,null,bounds")
void func2() { }

pragma(inline, true)
@no_sanitize("undefined")
void test_no_undefined()()
{
    counter++;
}

pragma(inline, true)
void test_sanitize()()
{
    counter++;
}

void func3()
{
  counter++;
  test_no_undefined();
  test_sanitize();
}

private __gshared int counter;

// { dg-final { scan-tree-dump-times "Function test_no_undefined" 1 "optimized" } }
// { dg-final { scan-tree-dump-times "Function test_sanitize" 0 "optimized" } }
