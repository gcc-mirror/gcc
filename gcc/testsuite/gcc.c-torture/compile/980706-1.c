void g(long long);

long long f(long long v1, long long v2, long long v3, long long v4)
{
  g(v1);
  g(v2);
  g(v3);
  g(v4);
  return v1 && v2;
} 
