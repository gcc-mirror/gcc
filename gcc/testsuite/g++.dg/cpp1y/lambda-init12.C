// PR c++/62241
// { dg-do compile { target c++14 } }

template<typename Signature>
struct function
{
  template<typename Functor>
  function(Functor) { }
};

int main ()
{
  int bar = 0;
  function<void ()> { [foo = bar] { } };
}
