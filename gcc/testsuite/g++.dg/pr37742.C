/* { dg-do compile } */

__extension__ typedef __SIZE_TYPE__ size_t;
void*   __valarray_get_memory(size_t __n);
int*__restrict__
__valarray_get_storage(size_t __n)
{
  return static_cast<int* __restrict__>(__valarray_get_memory(__n));
}

