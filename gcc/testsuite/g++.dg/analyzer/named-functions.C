#define NULL ((void *)0)

namespace my
{
  int socket (int, int, int);
};

void test_my_socket ()
{
  /* This shouldn't match the known function "::socket".  */
  my::socket (0, 0, 0); /* { dg-bogus "leak" } */
}
