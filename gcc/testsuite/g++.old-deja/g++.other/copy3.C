// { dg-do link  }
// Origin: ericp@mit.edu

class bar {
};

class foo {  
  foo (const foo &f);

public:
  
  foo (bar x) {}
  foo () {}
  
  void test (const foo &f) {}
};

int main (void) {
  foo f;
  bar b;

  f.test (b);
}
