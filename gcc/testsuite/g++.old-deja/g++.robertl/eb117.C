#include <stdio.h>
#include <assert.h>

struct F {
  int i;
};

F f;

int main( int, char** ) {

  int F:: *of;
  int *i = (int *) &of;
  of = &F::i;

  F *b = ((F*) ((int) &f.i - *i));
  F *a = &f;


  printf("%d\n", a-b);
  printf("%d\n", b-a);

  assert( (a-b) == -(b-a) );    // will fail with egcs-1.0

  return 0;
}
