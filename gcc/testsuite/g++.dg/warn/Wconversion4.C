// PR c++/45385
// { dg-options "-Wconversion" } 

void foo(unsigned char);

class Test
{
  void eval()
  {
    foo(bar());  // { dg-warning "may change value" }
  }

  unsigned int bar() const
  {
    return __INT_MAX__ * 2U + 1;
  }
};
