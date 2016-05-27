// PR c++/69855
// { dg-do compile }

int get();
void f() {
  char get(); // { dg-error "ambiguating" }
}

int get2();
char get2(int);
void f2() {
  char get2(); // { dg-error "ambiguating" }
}

char get3(int);
void f3() {
  char get3();
}

void f4() {
  char get4();
}
int get4(); // { dg-error "ambiguating" }

void get5();

template <class T> struct X
{
  void g()
  {
    int get5(); // { dg-error "ambiguating" }
  }
};


template <class T> struct X2
{
  void g()
  {
    int get6();
  }
};

void get6(); // { dg-error "ambiguating" }
