/* { dg-do compile } */
#include <altivec.h>
vector float
f(int i)
{
  switch (i) {
  case 0:
    return (vector float)(((vector unsigned char){3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3}));
  }
  return ((vector float){0,0,0,0});
}
