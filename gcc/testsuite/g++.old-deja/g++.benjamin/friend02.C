// { dg-do assemble  }
//980610 bkoz
// example 2: ok

class bar;
class foo {
public:
    int func(bar *);
    foo(){}
    ~foo(){}
};

class bar {
  int st;
public:
  bar(){st=12;}
  ~bar(){}
  friend int foo::func(bar *);
};

int foo::func(bar *obj) {
  obj->st++;
  return (obj->st);
}

void test02() {
  foo obj_f;
  bar obj_b;
  
  obj_f.func( &obj_b);
}
