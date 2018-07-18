extern "C" void *memset (void *, int, __SIZE_TYPE__);
void *p;

template <int T>
struct B
{
  void foo () { memset (p, 0, 4 * T * sizeof(float)); }
};

