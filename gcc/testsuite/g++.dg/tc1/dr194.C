// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR194: Identifying constructors 

struct A
{
  inline explicit A();
};

template <class>
struct B
{
  inline explicit B();
};

template struct B<void>;
