// { dg-do run  }
// From: bruno@isoft.com.ar (Bruno R. Depascale)
// Subject: No destructor bug
// Date: Mon, 14 Feb 1994 12:49:45 -0300 (Arg)

// Bug: temporaries created with constructor notation aren't destroyed.

int count = 0;

class A {
public:
  A() { ++count; }
  ~A() { --count; }
};

int main()
{
  A();
  return count;
}
