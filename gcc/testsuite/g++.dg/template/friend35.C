// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/4403: Incorrect friend class chosen during instantiation.

template <typename T>
struct A
{
  struct F;
};
 
template <typename T>
struct B : A<T>
{
  friend struct F;
private:
  int priv;
};
 
struct F
{
  void func(void)
  {
    B<int> b;
    b.priv = 0;
  }
};
