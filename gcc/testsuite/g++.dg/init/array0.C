// { dg-do compile }
// { dg-options "" }
// PR C++/24138

void foo()
{
  typedef struct {
    unsigned char dir;
    int data[0];
  } yanito;
  static const yanito horse = { 1,  { 2,  3 }  }; // { dg-error "too many" }
}
