struct
{
  char a, b, f[3];
} s;

long i = s.f-&s.b;
long long j = s.f-&s.b;
