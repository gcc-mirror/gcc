// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }
int main(void)
{
  alignof(void (void));   // { dg-warning "3:ISO C\\+\\+ does not permit .alignof. applied to a function type" }
}
