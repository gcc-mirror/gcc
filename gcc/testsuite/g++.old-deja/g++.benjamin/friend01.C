// { dg-do assemble  }
//980610 bkoz
// example 1: buggy

class foo {
public:
    class bar;
    int func(bar *);
    class bar {
        int st;
    public:
        bar(){st=12;}
        ~bar(){}
        friend int foo::func(bar *);
    };
    foo(){}
    ~foo(){}
};


int foo::func(bar *obj) {
  obj->st++;
  return (obj->st);
}

void test02() {
  foo obj_f;
  foo::bar obj_b;
  
  obj_f.func( &obj_b);
}
