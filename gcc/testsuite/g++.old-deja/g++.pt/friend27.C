// { dg-do assemble  }

class S
{
  friend void f<>(int); // { dg-error "" } does not match any template
  int i;
};
