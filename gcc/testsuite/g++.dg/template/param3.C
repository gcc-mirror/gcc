// PR c++/49418

template <class T>
void f (const T t)
{
  t = 1;			// { dg-error "" }
}

int main()
{
  f(1);
}
