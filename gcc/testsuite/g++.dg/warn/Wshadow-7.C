// PR c++/44128
// { dg-options "-Wshadow" }

typedef long My_ssize_t;  // { dg-warning "shadowed declaration" }
typedef int Foo;          // { dg-warning "shadowed declaration" }
struct Bar1 {             // { dg-bogus "shadowed declaration" }
  int a;
};
struct Bar2 {             // { dg-warning "shadowed declaration" }
  int a;
};

void func() {
  typedef int My_ssize_t; // { dg-warning "shadows a global" }
  typedef char My_Num;    // { dg-warning "shadowed declaration" }
  {
    typedef short My_Num; // { dg-warning "shadows a previous local" }
  }
  int Foo;                // { dg-warning "shadows a global" }
  float Bar1;             // { dg-bogus "shadows a global" }
  struct Bar2 {           // { dg-warning "shadows a global" }
    int a;
  };
  struct Bar3 {           // { dg-warning "shadowed declaration" }
    int a;
  };
  struct Bar4 {           // { dg-bogus "shadowed declaration" }
    int a;
  };
  {
    struct Bar3 {         // { dg-warning "shadows a previous local" }
      int a;
    };
    char Bar4;            // { dg-bogus "shadows a previous local" }
    int My_Num;           // { dg-warning "shadows a previous local" }
  }
}
