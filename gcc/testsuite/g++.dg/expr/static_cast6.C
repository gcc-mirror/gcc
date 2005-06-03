// PR c++/21853

struct blah {
  int a;
};

int main( int argc, char ** argv ) {
  int blah::* ptdma = &blah::a;

  const void *ptdmv = static_cast< void * >( &ptdma );

  int blah::* const ptdmb = * static_cast< int blah::* const * >( ptdmv );

  return 0;
}
