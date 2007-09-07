// { dg-options "-fvisibility=hidden" }
// { dg-require-visibility "" }
// { dg-final { scan-not-hidden "_ZGVZN5otherclEvE4s_cd" } }

extern "C" int printf (const char *, ...);

#define DLLEXPORT __attribute__ ((visibility("default")))

struct class_data
{
  int apple;
  class_data() { printf("non trivial ctor\n"); }
};

struct DLLEXPORT other
{
  class_data* operator ()()
  {
    static class_data s_cd;
    return &s_cd;
  }
};

int main()
{
  other aFoo;
  aFoo();
  return 0;
}
