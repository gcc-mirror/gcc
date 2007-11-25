/* Check character-returning bind(C) functions
   PR fortran/34079
   To be linked with bind_c_usage_16.f03
*/

#include <stdlib.h>

char returnA(char *);
char returnB(void);
void test(void);

int main()
{
  char c;
  c = 'z';
  c = returnA(&c);
  if (c != 'A') abort();
  c = returnB();
  if (c != 'B') abort();
  test();
  return 0;
}
