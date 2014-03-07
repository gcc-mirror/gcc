// Test for conversion from stateless lambda to function pointer.

// { dg-do run { target c++11 } }

typedef int (*pfn)(int);

int main()
{
  pfn p = [](int i) { return i-42; };
  return p (42);
}
