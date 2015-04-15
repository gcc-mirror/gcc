// { dg-do assemble  }

void f() 
{
  union {
  private:
    int i; // { dg-message "" } private
  } u;

  u.i = 3; // { dg-error "" } within this context
}
