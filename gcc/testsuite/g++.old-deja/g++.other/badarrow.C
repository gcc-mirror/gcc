// Build don't link:

struct S { 
  int i;
} s;

void f()
{
  s->i = 3; // ERROR - base operand
}
