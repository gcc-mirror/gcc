// PR c++/98659
// { dg-do compile }

template <bool> struct enable_if;
struct function {
  template <typename _F> void operator=(_F);
};
struct map {
  function operator[](int);
};
enum { E };
template <typename> void foo ();
template <typename T>
typename enable_if<T::value>::type foo ();

void
bar ()
{
  map m;
  m[E] = foo<int>;
}
