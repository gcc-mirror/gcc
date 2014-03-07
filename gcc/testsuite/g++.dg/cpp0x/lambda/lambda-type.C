// Every id-expression that is a use (_basic.def.odr_ 3.2) of an entity
// captured by copy is transformed into an access to the corresponding
// unnamed data member of the closure type.
//...
// Every occurrence of decltype((x)) where x is a possibly parenthesized
// id-expression that names an entity of automatic storage duration is
// treated as if x were transformed into an access to a corresponding data
// member of the closure type that would have been declared if x were a use
// of the denoted entity.

// So, other appearances of 'x' within decltype do not refer to the closure
// member, because they are not "use"s in the sense of 3.2.

// { dg-do compile { target c++11 } }

template<class T, class U>
struct same_type;
template <class T>
struct same_type<T,T> { };

int main()
{
  int i;
  [=] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int const&>();
    i+1;
    same_type<decltype((i)),int const&>();
    same_type<decltype(i),int>();
  };
  [=] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int const&>();
    same_type<decltype(i),int>();
  };
  [=] () mutable {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int &>();
    same_type<decltype(i),int>();
  };
  [&] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int &>();
    same_type<decltype(i),int>();
  };
  [i] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int const&>();
  };
  [&,i] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int const&>();
  };
  [i] () mutable {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int &>();
  };
  [&,i] () mutable {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int &>();
  };
  [&i] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int &>();
  };
  [=,&i] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int &>();
  };
  [] {
    same_type<decltype(i),int>();
    same_type<decltype((i)),int const&>(); // { dg-error "" "not captured" }
  };
}
