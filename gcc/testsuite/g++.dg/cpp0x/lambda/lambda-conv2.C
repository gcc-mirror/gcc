// Test for conversion from stateless lambda to function pointer.

// { dg-options -std=c++0x }
// { dg-do run }

typedef int (*pfn)(int);

int main()
{
  pfn p = [](int i) { return i-42; };
  return p (42);
}
