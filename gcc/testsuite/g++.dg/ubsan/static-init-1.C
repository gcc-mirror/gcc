// PR sanitizer/66190
// { dg-do compile }
// { dg-options "-fsanitize=null -std=c++11" }

class A {
public:
  void fn1 (int);
};

class G {
  ~G ();
  A t;
  virtual void fn2 () {
    static int a;
    static int &b = a;
    static int &c (a);
    static int &d {a};
    t.fn1 (b);
  }
};
G ::~G () {}
