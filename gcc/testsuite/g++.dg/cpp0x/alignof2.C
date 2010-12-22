// { dg-do compile }
// { dg-options "-std=c++0x -pedantic" }
int main(void)
{
  alignof(int); //ok with a type but not with an expression
  alignof(3);   // { dg-warning "alignof" }
}
