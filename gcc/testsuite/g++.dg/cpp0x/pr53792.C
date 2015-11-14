// PR c++/53792
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump "return 0" "optimized" } }

struct entry {
  char const* label;
  int         value;
};

constexpr bool same(char const *x, char const *y) {
  return !*x && !*y ? true
    : /* default */    (*x == *y && same(x+1, y+1));
}

constexpr int keyToValue(char const *label, entry const *entries) {
  return !entries->label ? entries->value
       : same(entries->label, label) ? entries->value
       : /*default*/                   keyToValue(label, entries+1);
}

constexpr entry foo[] = {{"Foo", 0}, {"Bar", 1}, {"FooBar", 2}, {0, -1}};

int
bar()
{
  int result = keyToValue("Foo", foo);
  return result;
}
