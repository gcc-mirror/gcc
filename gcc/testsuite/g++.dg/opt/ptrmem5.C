// PR tree-opt/18904
// { dg-do compile }
// { dg-options "-O3" }

struct Data;
struct Wrapper {
  Data* D;
};
struct Data {
  int X;
  void init(Wrapper&);
};
void Data::init( Wrapper &w ) {
  int Data::* res  = &Data::X;
  w.D = this;
  for( int i = 0; i < 4; i++ )
    (w.D->*res) = 0;
}

