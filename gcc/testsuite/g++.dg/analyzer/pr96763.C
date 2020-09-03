// { dg-do compile { target c++11 } }

struct c0;

struct md {
  int c0::*jj[2];
};

void
n0 ()
{
  md{};
}
