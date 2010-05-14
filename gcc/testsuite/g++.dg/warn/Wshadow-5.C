// Wshadows was giving warnings for nested function parameters in nested class
// or structure that we didn't want.
// { dg-do compile }
// { dg-options "-Wshadow" }

// PR c++/41825
int f (int n)
{
    int bar (int n) { return n++; } // { dg-error "a function-definition is not allowed here" }
    return bar (n); // { dg-error "was not declared in this scope" }
}

int g (int i)
{
    struct {
        int bar (int i) { return i++; } // { dg-bogus "shadows" }
    } s;

    return s.bar (i);
}

// PR c++/30566
void h( int x )
{
  class InnerClass
    {
      public:
              static int g( int x ) // { dg-bogus "shadows" }
                {
                  // empty
                }
    };
}
