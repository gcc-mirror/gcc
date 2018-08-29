#ifdef __cplusplus
struct A1 {};
struct A2 {};
struct dummy : A1, A2 {} ;
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
