/* { dg-options "-fcilkplus" } */
/* { dg-do run { target i?86-*-* x86_64-*-* arm*-*-* } } */
/* { dg-options "-fcilkplus -lcilkrts" { target { i?86-*-* x86_64-*-* arm*-*-* } } } */

#if HAVE_IO
#include <iostream>
#endif

class Some_Struct
{
  int calculated_value;
  short some_unused_value;
public:
  Some_Struct () {
      this->calculated_value = 0;
  }
  Some_Struct (int value) {
      this->calculated_value = value;
  }
  Some_Struct operator=(Some_Struct f) {
      this->calculated_value = f.calculated_value;
      return *this;
  }
  bool operator!=(Some_Struct f) {
      return (this->calculated_value != f.calculated_value);
  }
  Some_Struct operator+(Some_Struct &f) {
    Some_Struct z;
    z.calculated_value = this->calculated_value + f.calculated_value;
      return z;
  }
  Some_Struct operator-(int x) {
    Some_Struct z;
    z.calculated_value = this->calculated_value - x;
    return z;
  }
  bool operator<(int x) {
      return (this->calculated_value < x);
  }
  int get_calculated_value () {
      return this->calculated_value;
  }
};


template <class T>
T fibonacci_serial (T f)
{
  if (f < 2)
    return f;
  T a = fibonacci_serial (f-1);
  T b = fibonacci_serial (f-2);
  return (a+b);
}

template <class T>
T fibonacci (T f)
{
  if (f < 2)
    return f;
  T a = _Cilk_spawn fibonacci (f-1);
  T b = fibonacci (f-2);
  _Cilk_sync; 
  return (a+b);
}

int main (void)
{
  Some_Struct f (40), f_serial(40);
  f = fibonacci (f);
  f_serial = fibonacci_serial (f_serial);
  
  if (f != f_serial)
    __builtin_abort ();
  
  int t = 40, t_serial = 40;
  t = fibonacci (t);
  t_serial = fibonacci_serial (t_serial);
  if (t != t_serial)
    __builtin_abort ();

  short s = 20, s_serial = 20;
  s = fibonacci (s);
  s_serial = fibonacci_serial (s_serial);
  if (s != s_serial)
    __builtin_abort ();

#if HAVE_IO
  std::cout << "Fib_Parallel (40) = " << f.get_calculated_value() << std::endl;
  std::cout << "Fib_Serial   (40) = " << f_serial.get_calculated_value() 
    << std::endl;
#endif
  return 0;
}
