// PR c++/77321
// { dg-do compile { target c++14 } }
// { dg-options "-Wall" }
// { dg-require-effective-target size24plus }

extern "C" void *memset (void *, int, __SIZE_TYPE__);
extern "C" void *malloc(__SIZE_TYPE__);

struct S {
    char *a;
};

template <typename T>
void Test(T & Obj) {
    auto && a(Obj.a);
    a = (char*)::malloc(1024 * 1024);
    ::memset(a + 28, 'X', 6);
}

int main()
{
  S d;
  Test(d);
  return 0;
}
