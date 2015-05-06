// In C++98 mode this gets a -Wc++11-compat warning, in C++11 mode a
// -Wterminate warning.

// { dg-options "-Wall" }

struct A
{
  ~A()
  {
    throw 1;			// { dg-warning "terminate" }
  }
};

int main()
{
  try { A a;  }
  catch (...) {}
}


