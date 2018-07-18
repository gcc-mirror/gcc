#ifdef __cplusplus
struct A1
{
  void foo (void);
  unsigned int : 15;
};
struct A2
{
  void bar (void);
  unsigned int : 15;
};
struct dummy : A1, A2
{
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
