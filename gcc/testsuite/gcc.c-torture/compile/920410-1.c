int alloc_float(f)
 float f;
{  union
    {
      float f;
      int i;
    }
  u;
  u.f=f;
  return u.i&~1;
}

float c_float(int obj)
{  union
    {
      float f;
      int i;
    } u;

  u.i=obj;
  return u.f;
}

main()
{ int x=alloc_float(1.2);
  int y=alloc_float(5.7);
  int z=alloc_float(c_float(x)*c_float(y));

  printf("%g\n",(double)c_float(z));
}
