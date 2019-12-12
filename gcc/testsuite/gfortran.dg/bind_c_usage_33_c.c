#include <stdio.h>

void odopen(int*);

int main()
{
  int unit = 42;
  odopen(&unit);
  if (unit != 8)
    {
      fprintf(stderr,"wrong result");
      return 1;
    }
   return 0;
}
