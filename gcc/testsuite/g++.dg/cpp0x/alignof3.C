// { dg-do compile }
// { dg-options "-std=c++11 -pedantic" }
int main(void)
{
  alignof(void (void));   // { dg-warning "function type" }
}
