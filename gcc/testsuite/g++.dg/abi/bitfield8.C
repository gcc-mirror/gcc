// { dg-do run { target i?86-*-* } }
// { dg-options "-fabi-version=0" }
// { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } }

struct A { 
  virtual void f() {}
  int f1 : 1; 
};

struct B : public A {
  int f2 : 31;
  int f3 : 4; 
  int f4 : 3;
};

int main ()
{
  if (sizeof (B) != 16)
    return 1;
}
  
