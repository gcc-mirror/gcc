//Origin: benko@sztaki.hu
//PR c++/11432
// The mainline ICE on this one between 2003-01-16 and 2003-07-29.

// { dg-do compile }
 
 extern "C" void abort();
 
 
template <int A>
struct a
{
  static int const value = A - 1;
};

 
template <int B>
struct b
{
  static int foo()
  {
    return a<L>::value;
  }

 
  static int const L = a<B + 1>::value;
};
