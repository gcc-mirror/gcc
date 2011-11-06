// { dg-do compile }
// { dg-options "-std=c++0x -pedantic" }
int main(void)
{
  alignof(void (void));   // { dg-warning "function type" }
}
