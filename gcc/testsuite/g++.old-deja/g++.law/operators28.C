// Build don't link: 
// GROUPS passed operators
#include <sys/types.h>
#include <stdio.h>

class new_test
{
  int type;
public:
  void* operator new(size_t sz, int count, int type);
};

void* new_test::operator new(size_t sz, int count, int type)
{
  void *p;

  printf("%d %d %d\n", sz, count, type);

  p = new char[sz * count];
  ((new_test *)p)->type = type;
  return p;
};

int main()
{
  new_test *test;
  int count = 13;

  test = new(count, 1) new_test;
};
