/* { dg-do compile } */
/* { dg-options "-Os -fcaller-saves" } */

#include <stdint.h>

typedef struct {
  uint8_t x;
  uint32_t y;
} A;

A a;

extern int bar(int);
extern int foo (char *s, ...);

extern uint8_t param;
extern uint8_t h,m,s,ld,lm;
extern uint16_t d;

void gps_parse_string(int z)
{
  while (bar(z))
  {
    switch (param)
    {
      case 0: foo("a", &h, &m, &s, &d); break;
      case 1: foo("d", &ld, &lm, &a.y); break;
    }
  }
}
