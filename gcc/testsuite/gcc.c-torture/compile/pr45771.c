static const int data[2048];

void foo (void *ptr)
{
  __builtin_memcmp (data, ptr, 1);
}

