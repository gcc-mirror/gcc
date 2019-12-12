// { dg-do compile { target c++2a } }

template<typename T>
  concept Int = __is_same_as(T, int);

template<typename> struct Foo;

// Instantiation of default arguments happens at the point of
// instantiation for the class.

template<Int T = char> struct S1 { };
template<Int auto X = false> struct S2 { };

S1<> s1; // { dg-error "constraint failure" }
S2<> s2; // { dg-error "placeholder constraints not satisfied" }
