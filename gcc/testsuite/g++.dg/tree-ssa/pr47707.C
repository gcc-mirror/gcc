/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vrp" } */
#include <assert.h>

struct CH
{
  unsigned char ch : 3;
} ch;

__attribute__((noinline)) void MakeCheckOp (unsigned int *v1, unsigned int *v2)
{
 assert (*v1 == *v2);

}

int main (void)
{

  int len;

  for (len = 4; len >= 1; len--)
  {
     unsigned v1, v2;
     ch.ch = len;
     v1 = ch.ch;
     v2 = len;
     MakeCheckOp (&v1, &v2);
  }
}
