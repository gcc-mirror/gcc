// { dg-do assemble  }

struct S
{
  friend void f<>(int); // { dg-error "" } does not match any template
};
