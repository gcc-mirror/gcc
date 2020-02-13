// PR c++/93643

void* callback(const char* name);

extern "C" {

  inline void f1()
  {
    static void (*f)();
    f = (void(*)()) callback("f1");
    f();
  }

  inline void f2()
  {
    static void (*f)();
    f = (void(*)()) callback("f2");
    f();
  }

} // extern "C"

int main()
{
  f1();
  f2();
}
