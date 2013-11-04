/* { dg-do compile } */
/* { dg-options "-Os -mmcu=atmega8" } */

typedef unsigned char uint8_t;
typedef unsigned int uint16_t;

extern uint8_t f1 (const uint8_t*);
extern void f2 (uint8_t*, uint8_t);

void func (uint16_t parameter, uint8_t *addr, uint8_t data)
{
   uint8_t status;

   status = f1 (addr + 8);

   addr++;

   if (*addr == parameter + 8)
      *addr = parameter;

   f2 (addr, data);
   f2 (addr + 8, status + 1);
}
