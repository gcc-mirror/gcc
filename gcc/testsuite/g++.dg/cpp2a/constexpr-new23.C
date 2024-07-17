// PR c++/115645
// { dg-do compile { target c++20 } }

using size_t = decltype(sizeof(0));

void* operator new(size_t, void* p) { return p; }
void* operator new[](size_t, void* p) { return p; }

#define VERIFY(C) if (!(C)) throw

namespace std {
  template<typename T>
    constexpr T* construct_at(T* p)
    {
      if constexpr (__is_array(T))
        return ::new((void*)p) T[1]();
      else
        return ::new((void*)p) T();
    }
}

constexpr void
test_array()
{
  int arr[1] { 99 };
  std::construct_at(&arr);
  VERIFY( arr[0] == 0 );

  union U {
    long long x = -1;
    int arr[4];
  } u;

  auto p = std::construct_at(&u.arr);
  VERIFY( (*p)[0] == 0 );
}

static_assert( [] { test_array(); return true; }() );
