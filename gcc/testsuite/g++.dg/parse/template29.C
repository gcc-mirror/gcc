// PR c++/24314

// The base template.
template <class T>
struct A
{
   int select() { return 0; }
};

//Extra "template<>"
template <>
template <>
template <>
template <>
template <>
template <>
template <>
template <>
template <>
template <>
template <>
template <class T>
struct A<T*>  // { dg-error "too many template-parameter-lists" }
{
   int select() { return 1; }
};

int main()
{
   return A<int*>().select();
}
