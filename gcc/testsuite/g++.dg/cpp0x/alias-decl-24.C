// Origin: PR c++/53540
// { dg-do compile { target c++11 } }

template <typename T>
struct context
{
  typedef int type;
};

template <typename T>
void function()
{
  using ctx1 = context<T>;
  typename ctx1::type f1;

  typedef context<T> ctx2;
  typename ctx2::type f2;
}

int main()
{
  function<int>();
}

