// { dg-do run  }
// prms-id: 3570

extern "C" int printf(const char *, ...);

struct A {
   void print() {printf("A");};
};

struct B :  A {
   typedef A superB;
   void print() {superB::print(); printf("B");};
};

struct C :  B {
   typedef B superC;
   void print() {superC::print(); printf("C");};
};

int main ()
{
   A a;
   B b;
   C c;

   a.print(); printf("\n");
   b.print(); printf("\n");
   c.print(); printf("\n");
   return 0;
}
