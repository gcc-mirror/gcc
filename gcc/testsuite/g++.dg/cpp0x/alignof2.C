// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }
int main(void)
{
  alignof(int); //ok with a type but not with an expression
  alignof(3);   // { dg-warning "alignof" }
}
