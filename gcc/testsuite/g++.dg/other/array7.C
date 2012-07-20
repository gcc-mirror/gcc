// PR c++/54038

extern const char *const v[];
typedef char T;
void foo (const T *const[]);
struct A
{
  static const char *const a[];
};
