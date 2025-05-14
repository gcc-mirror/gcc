// Test that we don't bother to instantiate add since there were errors in
// checked_add.

template <class T> T add (T t) { return t+1; } // { dg-bogus "no match" }

template <class T> T checked_add (T t)
{
  add (t);
  return t+1;			// { dg-error "no match" }
}

struct A { };

int main()
{
  checked_add (A());
}
