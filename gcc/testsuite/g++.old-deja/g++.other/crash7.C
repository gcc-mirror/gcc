// { dg-do assemble  }

void f() 
{
  union {
  private:
    int i; // { dg-error "" } private
  } u;

  u.i = 3; // { dg-error "" } within this context
}
