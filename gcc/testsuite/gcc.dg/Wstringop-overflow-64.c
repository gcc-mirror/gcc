/* PR middle-end/92936 - missing warning on a past-the-end store to a PHI
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

typedef __SIZE_TYPE__ size_t;

void* malloc (size_t);
void* memset (void*, int, size_t);

extern char a3[3], a5[5], a9[9];

extern int cnd[];

void* f2 (void)
{
  char *p0 = cnd[0] ? a3 : 0;
  char *p1 = cnd[1] ? a5 : p0;

  return memset (p1, 0, 6);   // { dg-warning "writing 6 bytes into a region of size 5" }
}

void* f3 (void)
{
  char *p0 = cnd[0] ? a3 : 0;
  char *p1 = cnd[1] ? a5 : 0;
  char *p2 = cnd[2] ? p0 : p1;

  return memset (p2, 0, 6);   // { dg-warning "writing 6 bytes into a region of size 5" }
}

void* f3_2 (void)
{
  char *p0 = cnd[0] ? a3 : 0;
  char *p1 = cnd[1] ? a5 : 0;
  char *p2 = cnd[2] ? p1 : p0;

  return memset (p2, 0, 6);   // { dg-warning "writing 6 bytes into a region of size 5" }
}

void* f3_3 (void)
{
  char *p0 = cnd[0] ? a5 : 0;
  char *p1 = cnd[1] ? p0 : a5;
  char *p2 = cnd[2] ? p1 : p0;

  return memset (p2, 0, 6);   // { dg-warning "writing 6 bytes into a region of size 5" }
}

void* f4 (void)
{
  char *p0 = cnd[0] ? a3 : 0;
  char *p1 = cnd[1] ? a5 : 0;
  char *p2 = cnd[2] ? p0 : 0;
  char *p3 = cnd[3] ? p1 : p2;

  return memset (p3, 0, 6);   // { dg-warning "writing 6 bytes into a region of size 5" }
}

void* f9 (void)
{
  char *p0 = cnd[0] ? a5 : 0;
  char *p1 = cnd[1] ? a5 + 1 : 0;
  char *p2 = cnd[2] ? a5 + 2 : 0;
  char *p3 = cnd[3] ? a5 + 3 : 0;
  char *p4 = cnd[4] ? a5 + 4 : 0;

  char *p5 = cnd[5] ? p0 : p1;
  char *p6 = cnd[6] ? p5 : p2;
  char *p7 = cnd[7] ? p6 : p3;
  char *p8 = cnd[8] ? p7 : p4;
  char *p9 = cnd[9] ? p8 : p5;

  return memset (p9, 0, 6);   // { dg-warning "writing 6 bytes into a region of size 5" }
}
