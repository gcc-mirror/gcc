// PR c++/50757
// { dg-options "-Wformat -Wno-nonnull" }

extern void *f (void *__s) __attribute__ ((__nonnull__ (1)));

int main()
{
  void* const s = 0;
  f(s);
}
