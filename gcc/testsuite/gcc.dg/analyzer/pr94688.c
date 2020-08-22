int a, b;
void d();
void c()
{
  d((void (*)()) & a + b);
}
