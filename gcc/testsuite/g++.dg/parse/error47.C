struct T { };

T foo();

void bar(int a, int b)
{
  if (foo() && a < b) // { dg-error "13:no match for 'operator&&'" }
    ;
}
