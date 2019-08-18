// PR c++/65143
// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "vptr" 1 gimple } }

struct A
{
  int i();
};

struct B : public virtual A
{
  int get()
  {
    return A::i() + 1;
  }
};

struct C final : public B
{
  int get()
  {
    return A::i() + 2;
  }
};

int foo(C& c)
{  
  return c.get(); // Need not go via vtable pointer as class C is final
}

int foo(B& b2)
{
  return b2.get(); // This has to go via vtable as most derived class can change the location of A
}
