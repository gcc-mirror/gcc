// N3648: capture init at non-block scope
// { dg-options "-std=c++1y -w" }
// { dg-do run }

int i = 42;
int j = [x=i]{ return x; }();

int main()
{
  if (j != 42) __builtin_abort();
}
