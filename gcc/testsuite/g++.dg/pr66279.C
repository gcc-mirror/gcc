// { dg-do run }

struct A {};

struct B : public virtual A
{
  B();
};

B::B()
{
  unsigned int x = 42;

  __asm__ __volatile__ ("" : "+r"(x));

  if (x != 42)
    __builtin_abort ();
}

int main()
{
  B b;
}
