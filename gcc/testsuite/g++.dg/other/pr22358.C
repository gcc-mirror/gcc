/* { dg-do compile } */

struct a
{
  virtual ~a();
};
struct b : virtual a { };
b a11;
