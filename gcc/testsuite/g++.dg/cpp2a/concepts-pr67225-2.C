// PR c++/67225
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<typename Target>
// template<typename Target, typename... Ts>
concept bool has_resize ()
{
  return requires (Target tgt)
  {
    { tgt.resize () };
  };
}

template<typename Target>
void resize (Target tgt)
{
  if constexpr (has_resize<Target> ())
  {
    tgt.resize ();
  }
}

class MyClass
{
  private:
    int foo (int i)
    {
      return i * 2;
    }
};

int main ()
{
  return MyClass {}.foo (7); // { dg-error "private within this context" }
}
