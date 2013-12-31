/* Test that various types are all derived from int.  */
/* { dg-do compile } */

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

extern size_t a;
unsigned int a;
extern unsigned int aa;
size_t aa;

extern ssize_t b;
int b;
extern int bb;
ssize_t bb;

extern ptrdiff_t c;
int c;
extern int cc;
ptrdiff_t cc;

extern intptr_t d;
int d;
extern int dd;
intptr_t dd;

extern uintptr_t e;
unsigned int e;
extern unsigned int ee;
uintptr_t ee;



