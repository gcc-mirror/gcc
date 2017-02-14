/* { dg-do run } */
/* { dg-options "-O2" }  */

#include <string.h>

struct table_s
    {
    void (*fun0)
        ( void );
    void (*fun1)
        ( void );
    void (*fun2)
        ( void );
    void (*fun3)
        ( void );
    void (*fun4)
        ( void );
    void (*fun5)
        ( void );
    void (*fun6)
        ( void );
    void (*fun7)
        ( void );
    } table;

void callback0(){__asm("mov r0, r0 \n\t");}
void callback1(){__asm("mov r0, r0 \n\t");}
void callback2(){__asm("mov r0, r0 \n\t");}
void callback3(){__asm("mov r0, r0 \n\t");}
void callback4(){__asm("mov r0, r0 \n\t");}

void test (void) {
    memset(&table, 0, sizeof table);

    asm volatile ("" : : : "r3");

    table.fun0 = callback0;
    table.fun1 = callback1;
    table.fun2 = callback2;
    table.fun3 = callback3;
    table.fun4 = callback4;
    table.fun0();
}

void foo (void)
{
  __builtin_abort ();
}

int main (void)
{
  unsigned long p = (unsigned long) &foo;
  asm volatile ("mov r3, %0" : : "r" (p));
  test ();

  return 0;
}
