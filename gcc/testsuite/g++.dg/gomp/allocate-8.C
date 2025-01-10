/* { dg-do compile { target c++11 } } */
#include "allocate-allocator-handle.h"

/* I had wanted to simply include /include/gomp-constants.h to ensure
   synchronization, but including files from that directory does not seem
   to be supported.  */
#define GOMP_OMP_PREDEF_ALLOC_MAX	8
#define GOMP_OMPX_PREDEF_ALLOC_MIN	200
#define GOMP_OMPX_PREDEF_ALLOC_MAX	200

/* Test that all predefined allocators are correctly treated as predefined.  */

template<omp_allocator_handle_t Alloc>
void test_predefined_alloc()
{
  static int a = 42;
  #pragma omp allocate(a) allocator(Alloc)
}

/* Because this is written to work as far back as c++11 it is a little bit
   crusty.  It is metaprogrammed to automatically test the full ranges
   specified above.  */

template<omp_allocator_handle_t...>
struct sequence {};

template<__UINTPTR_TYPE__ Offset, __UINTPTR_TYPE__... Is>
using modified = sequence<static_cast<omp_allocator_handle_t>(Is + Offset)...>;

template<__UINTPTR_TYPE__ Start, __UINTPTR_TYPE__ End>
using make_offset_sequence = modified<Start, __integer_pack(End - Start)...>;

template<omp_allocator_handle_t... Allocs>
void unpack(sequence<Allocs...>)
{
  int helper[] = {(test_predefined_alloc<Allocs>(), 0)...};
}

void do_tests()
{
  /* make_sequence creates a sequence [Start, End) while the *_MAX values are
     inclusive, add 1 to the End arg to create an exclusive range.  */
  unpack(make_offset_sequence<1, GOMP_OMP_PREDEF_ALLOC_MAX + 1>{});
  unpack(make_offset_sequence<GOMP_OMPX_PREDEF_ALLOC_MIN, GOMP_OMPX_PREDEF_ALLOC_MAX + 1>{});
}
