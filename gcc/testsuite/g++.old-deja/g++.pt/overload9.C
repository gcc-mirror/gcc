template <class T>
int f(T)
{
  return 1;
}


template <class T>
int f(T*)
{
  return 0;
}


int main()
{
  int (*h)(int*) = &f;
  int (&k)(int*) = f;

  return (*h)(0) || (*k)(0);
}
