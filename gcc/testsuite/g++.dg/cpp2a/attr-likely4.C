// { dg-do compile { target c++20 } }

int a, b, c;

void
__attribute__((noinline))
bar()
{
  if (a == 123)
    [[likely]] c = 5;		// { dg-warning "both" }
  else
    [[likely]] b = 77;
}

int main()
{
  bar ();
  return 0;
}
