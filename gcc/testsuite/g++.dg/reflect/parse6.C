// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

template <info R>
bool f()
{
   return [: R :]<42>0;
}

int i;
int main()
{
   f<^^i>();
}
