// { dg-do run  }
// Bug: g++ has trouble copying anonymous structs.

typedef struct { int i; } foo;
struct A : public foo { 
  struct { int i; } x;
};

int main ()
{
  A a;
  a.i = 5;
  a.x.i = 42;
  A b (a);
  a = b;
  if (a.i != 5 || a.x.i != 42)
    return 1;
  return 0;
}
