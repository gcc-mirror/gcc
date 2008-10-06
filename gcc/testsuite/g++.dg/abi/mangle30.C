// Test for mangling of template args in a typename type.

struct A
{
  template <class T>
  struct B
  {
    typedef T myT;
  };
};

struct C {};

template <class T>
void f (T t, typename T::template B<C>::myT u, typename T::template B<int>::myT v);

int main()
{
  f (A(), C(), 1);
}

// { dg-final { scan-assembler "_Z1fI1AEvT_NS1_1BI1CE3myTENS2_IiE3myTE" } }
