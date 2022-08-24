/* PR middle-end/97373 - missing warning on sprintf into allocated destination
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" }
   { dg-require-effective-target alloca } */

#include "../range.h"

extern void* alloca (size_t);
extern void* malloc (size_t);

extern int sprintf (char*, const char*, ...);
#define sprintf(d, ...) (sprintf (d, __VA_ARGS__), sink (d))

void sink (void*, ...);

void test_alloca_range (void)
{
  int n1_2 = UR (1, 2);
  int n5_9 = UR (5, 9);

  char *d = (char*)alloca (n5_9);

  sprintf (d, "%i", 12345);

  d += n1_2;
  sprintf (d, "%i", 12345);

  d += n1_2;
  sprintf (d, "%i", 12345);

  d += n1_2;
  sprintf (d, "%i", 12345);

  d += n1_2;
  sprintf (d, "%i", 12345);         // { dg-warning "writing a terminating nul past the end of the destination" }

  d += n1_2;
  sprintf (d, "%i", 12345);         // { dg-warning "'%i' directive writing 5 bytes into a region of size 4" }
}


void test_malloc_range (void)
{
  int n2_3 = UR (2, 3);
  int n5_9 = UR (5, 9);

  char *d = (char*)malloc (n5_9);

  sprintf (d, "%i", 12345);

  d += n2_3;
  sprintf (d, "%i", 12345);

  d += n2_3;
  sprintf (d, "%i", 12345);         // { dg-warning "writing a terminating nul past the end of the destination" }

  d += n2_3;
  sprintf (d, "%i", 12345);         // { dg-warning "'%i' directive writing 5 bytes into a region of size 3" }
}


void test_vla_range (void)
{
  int n3_4 = UR (3, 4);
  int n5_9 = UR (5, 9);

  char vla[n5_9];
  char *d = vla;

  sprintf (d, "%i", 12345);

  d += n3_4;
  sprintf (d, "%i", 12345);

  d += n3_4;
  sprintf (d, "%i", 12345);         // { dg-warning "'%i' directive writing 5 bytes into a region of size 3" }
}
