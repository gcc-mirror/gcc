// { dg-do compile }

class A
{
};
template <class type> struct D:A
{
  type & operator[](int);
};
struct B
{
  typedef D <int *>Row;
  struct C
    {
      Row *row;
    };
};
B::C a;
B::Row & b = *a.row;
void
fn1 ()
{
  while (1)
    b[0] = b[0] ? (int *) -1 : 0;
}
