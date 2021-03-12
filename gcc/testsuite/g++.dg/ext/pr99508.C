// PR 99508, local exerns with aliased names
// { dg-do link }
int foo()
{
  void bar() asm ("bar_assembler");
  extern unsigned buzz asm("buzz_assembler");
  bar();
  return buzz;
}

void ALIASbar () asm ("bar_assembler");
void ALIASbar ()
{
}

unsigned ALIASbuz asm ("buzz_assembler") = 5;

int main ()
{
}
