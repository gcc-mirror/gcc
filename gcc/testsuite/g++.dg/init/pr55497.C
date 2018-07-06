// PR c++/55497
// { dg-options "-g" }
// { dg-require-effective-target alloca }

int get();

struct B
{
  B()
  {
    static const int v = get(); 
    char BUFFER[v];
  }
};

B b;
