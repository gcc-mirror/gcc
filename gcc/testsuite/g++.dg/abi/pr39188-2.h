template<typename T>
T
f (T x)
{
  static union 
    {
      T i;
    };
  T j = i;
  i = x;
  return j;
}
