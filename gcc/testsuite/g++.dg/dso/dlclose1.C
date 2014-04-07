// PR c++/60731
// { dg-do run { target dlopen } }
// { dg-add-options dlopen }
// { dg-build-dso "dlclose1-dso.cc" }

#include <dlfcn.h>
extern "C" void abort();
extern "C" int printf (const char *, ...);

// Open and close the DSO for each call so that statics are reinitialized.
int call()
{
  void *h = dlopen ("./dlclose1-dso.so", RTLD_NOW);
  if (!h) { printf ("dlopen failed: %s\n", dlerror()); abort(); }
  int (*fn)() = (int(*)())dlsym (h, "fn");
  if (!fn) { printf ("dlsym failed: %s\n", dlerror()); abort(); }
  int r = fn();
  dlclose (h);
  return r;
}

int main() {
  int i = call();
  int j = call();
  if (i != j)
    {
      printf ("mismatch: %d != %d\n", i, j);
      abort();
    }
}
