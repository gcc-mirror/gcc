// { dg-do compile { target c++11 } }
// { dg-additional-options "-O1 -Wno-pedantic" }
// PR c++/83187 ICE in get_alias_set due to canonical type confusion.

extern "C" {
  double cos (double);
  double sin (double);
}

template <typename> class COMPLEX;

template <>
struct COMPLEX<double>
{
  COMPLEX(double r, double i);

  __complex__ mem;
};

COMPLEX<double>::COMPLEX (double r, double i)
  : mem {r, i} {}

typedef double dbl_t;

dbl_t var;

void foo (COMPLEX<double> *ptr)
{
  const dbl_t unused = var;

  *ptr = COMPLEX<double> (cos (var), sin (var));
}
