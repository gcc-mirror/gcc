// { dg-do assemble  }

extern "C" void realloc();

class bug {
public:
  void realloc(int foo,int bar);
};

void f() {
  bug c;
  c.realloc(50,50);
}
 
