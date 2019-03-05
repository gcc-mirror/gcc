// PR c++/87476
// { dg-do compile { target c++11 } }

template <int>
struct S {
  void operator () () { constexpr unsigned char p[1] {}; }
};

void
foo ()
{
  S<0>{} ();
}
