// { dg-do compile { target c++20 } }

struct S
{
  union {
    char buf[8];
    char* ptr;
  };
  unsigned len;

  constexpr S(const char* s, unsigned n)
  {
    char* p;
    if (n > 7)
      p = ptr = new char[n+1];
    else
      p = buf;
    for (len = 0; len < n; ++len)
      p[len] = s[len];  // { dg-error "accessing uninitialized member" }
    p[len] = '\0';
  }

  constexpr ~S()
  {
    if (len > 7)
      delete[] ptr;
  }
};

constexpr bool test1()
{
  S s("test", 4);  // { dg-message "in .constexpr. expansion" }
  return true;
}

constexpr bool a = test1();  // { dg-message "in .constexpr. expansion" }


constexpr bool test2()
{
  S s("hello world", 11);
  return true;
}

constexpr bool b = test2();
