// { dg-do assemble  }

struct S { 
  int i;
} s;

void f()
{
  s->i = 3; // { dg-error "" } base operand
}
