// PR c++/115430
// { dg-do compile }

template<typename T>
int Func(T);
typedef int (*funcptrtype)(int);
funcptrtype fp0 = &Func<int>;
funcptrtype fp1 = +&Func<int>;
funcptrtype fp2 = (0, &Func<int>);
funcptrtype fp3 = (0, +&Func<int>);
funcptrtype fp4 = (0, 1, &Func<int>);

template<typename T>
void
g ()
{
  funcptrtype fp5 = (0, &Func<T>);
}

void
f ()
{
  g<int>();
}
