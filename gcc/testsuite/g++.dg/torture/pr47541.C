/* { dg-do run } */

struct Dummy {};
struct RefCount : public Dummy {
    ~RefCount(); /* Has to be non-pod.  */
    int *a;
    int *b;
};
RefCount::~RefCount(){}
struct Wrapper : public Dummy { RefCount ref; };
void __attribute__((noinline,noclone))
Push(Wrapper ptr)
{
  *ptr.ref.b = 0;
}
extern "C" void abort (void);
int main()
{
  int a = 1, b = 1;
  Wrapper x;
  x.ref.a = &a;
  x.ref.b = &b;
  Push(x);
  if (b != 0)
    abort ();
  return 0;
}
