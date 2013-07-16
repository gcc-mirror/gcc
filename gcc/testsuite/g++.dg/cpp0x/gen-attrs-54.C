// PR c++/56859
// { dg-require-effective-target c++11 }

template<unsigned size, unsigned alignment>
struct aligned_storage
{
  using type = struct { alignas(alignment) unsigned char data[size]; };
};

#define SA(X) static_assert((X),#X)
SA(alignof(aligned_storage<8,1>::type) == 1);
SA(alignof(aligned_storage<8,2>::type) == 2);
SA(alignof(aligned_storage<8,4>::type) == 4);
SA(alignof(aligned_storage<8,8>::type) == 8);
