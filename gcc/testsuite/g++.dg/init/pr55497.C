// PR c++/55497
// { dg-options "-g" }

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
