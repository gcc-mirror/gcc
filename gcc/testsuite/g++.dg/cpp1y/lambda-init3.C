// N3648: capture init at non-block scope
// { dg-options "-w" }
// { dg-do run { target c++1y } }

int i = 42;
int j = [x=i]{ return x; }();

int main()
{
  if (j != 42) __builtin_abort();
}
