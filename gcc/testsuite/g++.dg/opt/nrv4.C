// PR optimization/7145
// Bug: The NRV optimization caused us to lose the initializer for 'ret'.
// { dg-options -O }
// { dg-do run }

struct GdkColor {
  long  pixel;
  short red;
  short green;
  short blue;
};

inline GdkColor mkcolor() {
  GdkColor ret={0,1,2,3};
  return ret;
}

int
main()
{
  GdkColor col=mkcolor();
  return (col.pixel != 0 || col.red != 1 || col.green != 2 || col.blue != 3);
}
