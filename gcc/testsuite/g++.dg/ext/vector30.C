// PR c++/70430
// { dg-do run }
extern "C" void abort (void);
typedef int v4si __attribute__ ((vector_size (16)));
int main()
{
  v4si b = {1,0,-1,2}, c;
  c = b && 1;
  if (c[0] != -1 || c[1] != 0 || c[2] != -1 || c[3] != -1)
    abort ();
  c = b && 0;
  if (c[0] != 0 || c[1] != 0 || c[2] != 0 || c[3] != 0)
    abort ();
  return 0;
}
