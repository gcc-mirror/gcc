int global = 12;

int function1(void);

int function2(int a) // { dg-warning "Start fndef function2" }
{
  return function1() + a;
} //  { dg-warning "Finish fndef function2" }

int function1(void) // { dg-warning "Start fndef function1" }
{
  return global + 1;
} //  { dg-warning "Finish fndef function1" }
