// { dg-do compile }

import gcc.attributes;

void* my_calloc(size_t num, size_t size) @allocSize(1, 0)
{
    return null;
}

void* my_malloc(int a, int b, size_t size, int c) @allocSize(2)
{
    return null;
}
