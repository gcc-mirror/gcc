// Build don't link:

class foo {
public:
    class bar;
    void func(bar *);
    class bar {
      int st;
      friend void foo::func(bar *);
    };
};


void foo::func(bar *obj) {
  obj->st++;
}

void test02() {
  foo obj_f;
  foo::bar obj_b;
  
  obj_f.func( &obj_b);
}
