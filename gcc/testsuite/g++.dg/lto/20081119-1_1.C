#include "20081119-1.h"

__gnu_cxx::new_allocator<int> X;

int
f (__gnu_cxx::new_allocator<int> a)
{
 return a.max_size ();
}
