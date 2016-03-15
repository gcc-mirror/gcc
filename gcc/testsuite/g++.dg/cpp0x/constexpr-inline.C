// PR c++/53792 - [C++11] improving compiler-time constexpr evaluation
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O1 -fdump-tree-optimized" }

struct entry
{
  char const* label;
  int value;
};

constexpr bool same (char const *x, char const *y)
{
  return !*x && !*y ? true : /* default */ (*x == *y && same (x + 1, y + 1)); 
}

constexpr int
keyToValue (char const *label, entry const *entries)
{ 
  return !entries->label ? entries->value 
                         : same (entries->label, label) ? entries->value
                         : /* default */ keyToValue (label, entries + 1); 
}

constexpr entry foo[] = {{"Foo", 0}, {"Bar", 1}, {"FooBar", 2}, {0, -1}};

int bar ()
{
  int result = keyToValue ("Foo", foo); 
  return result;
}

int baz ()
{
  constexpr int result = keyToValue ("Foo", foo); 
  return result;
}

// Verify that the call to the keyToValue() constexpr function is inlined
// regardless of whether or not it's invoked in a constexpr expression.
// { dg-final { scan-tree-dump-not "keyToValue" "optimized" } }
