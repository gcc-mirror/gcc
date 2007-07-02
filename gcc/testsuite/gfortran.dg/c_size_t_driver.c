#include <stdlib.h>
void sub0(int my_c_size);

int main(int argc, char **argv)
{
  int my_c_size;

  my_c_size = (int)sizeof(size_t);
  sub0(my_c_size);

  return 0;
}
