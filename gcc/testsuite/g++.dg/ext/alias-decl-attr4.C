// { dg-do run { target c++11 } }

using global_vector_type  __attribute__((vector_size(16))) = float;

template <class T> struct A
{
    using type = T;
};

template < typename Val > struct S
{
    using vector_type __attribute__((vector_size(16))) =
        typename A<Val>::type;
        typedef Val vector_type2 __attribute__((vector_size(16)));
    int pr_size() { return sizeof(vector_type); }
    int pr_size2() { return sizeof(vector_type2); }
};

int main()
{
  if (sizeof (S<float>::vector_type) != sizeof (global_vector_type))
    return 1;
  if (sizeof (S<float>::vector_type2) != sizeof (global_vector_type))
    return 2;

  S<float> x;
  if (x.pr_size() != sizeof (global_vector_type))
    return 3;
  if (x.pr_size2() != sizeof (global_vector_type))
    return 4;
  
  return 0;
}
