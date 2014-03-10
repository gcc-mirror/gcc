// { dg-do compile { target c++11 } }

// From N2235

// function template 1
template<typename T>
  constexpr int bytesize(T t)
  { return sizeof (t); }        // OK

char buf[bytesize(0)];          // OK -- not C99 VLA


// function template 2
template<typename _Tp>
  constexpr _Tp
  square(_Tp x) { return x; }

// Explicit specialization
template<>
  constexpr unsigned long
  square(unsigned long x) { return x * x; }

// Explicit instantiation
template int square(int);

class A { };
template A square(A);

template long square(long);
