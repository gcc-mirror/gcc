#ifdef __cplusplus
struct dummy
{
  virtual void bar (void) { }
  unsigned int : 15;
};
#else
struct dummy {};
#endif

struct foo
{
  int i1;
  int i2;
  int i3;
  int i4;
  int i5;
};
