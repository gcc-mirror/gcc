typedef char __attribute__ ((vector_size (4))) v4qi;
void retv (int a,int b,int c,int d, v4qi *ret)
{
  v4qi v = { a, b , c, d };
  *ret = v;
}
void retv2 (int a,int b,int c,int d, v4qi *ret)
{
  v4qi v = { a, b , c, d };
  *ret = v;
}
