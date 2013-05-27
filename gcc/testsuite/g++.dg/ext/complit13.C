// PR c++/10207
// { dg-options "" }

typedef struct { } EmptyStruct;
typedef struct { EmptyStruct Empty; } DemoStruct;

void Func()
{
  DemoStruct Demo;
  Demo.Empty = (EmptyStruct) {};
}
