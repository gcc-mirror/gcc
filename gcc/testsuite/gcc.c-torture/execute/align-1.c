typedef int new_int __attribute__ ((aligned(16)));
struct S { int x; };
 
int main()
{
  if (sizeof(struct S) != sizeof(int))
    abort ();
  return 0;
}
