// { dg-do assemble  }

struct S { 
  int i;
} s;

void f()
{
  s->i = 3; // { dg-error "4:base operand of .->. has non-pointer type .S." } base operand
}
