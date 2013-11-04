// { dg-options -std=c++11 }

#define assert(E) if(!(E))__builtin_abort();

#define m(x) 0

int
main()
{
  int i = m(1'2)+(3'4);
  assert(i == 0);
}
