#include <stdlib.h>

/* All the function pointers are declared and initialized in
   mips16-stubs-2.c.  */

extern double the_result;

extern void v0 (void);
extern void v1 (float);
extern void v5 (float, float);
extern void v9 (float, double);
extern void v2 (double);
extern void v6 (double, float);
extern void v10 (double, double);

extern float f0 (void);
extern float f1 (float);
extern float f5 (float, float);
extern float f9 (float, double);
extern float f2 (double);
extern float f6 (double, float);
extern float f10 (double, double);

extern double d0 (void);
extern double d1 (float);
extern double d5 (float, float);
extern double d9 (float, double);
extern double d2 (double);
extern double d6 (double, float);
extern double d10 (double, double);

extern _Complex float cf0 (void);
extern _Complex float cf1 (float);
extern _Complex float cf5 (float, float);
extern _Complex float cf9 (float, double);
extern _Complex float cf2 (double);
extern _Complex float cf6 (double, float);
extern _Complex float cf10 (double, double);

extern _Complex double cd0 (void);
extern _Complex double cd1 (float);
extern _Complex double cd5 (float, float);
extern _Complex double cd9 (float, double);
extern _Complex double cd2 (double);
extern _Complex double cd6 (double, float);
extern _Complex double cd10 (double, double);

extern void (*pv0) (void);
extern void (*pv1) (float);
extern void (*pv5) (float, float);
extern void (*pv9) (float, double);
extern void (*pv2) (double);
extern void (*pv6) (double, float);
extern void (*pv10) (double, double);

extern float (*pf0) (void);
extern float (*pf1) (float);
extern float (*pf5) (float, float);
extern float (*pf9) (float, double);
extern float (*pf2) (double);
extern float (*pf6) (double, float);
extern float (*pf10) (double, double);

extern double (*pd0) (void);
extern double (*pd1) (float);
extern double (*pd5) (float, float);
extern double (*pd9) (float, double);
extern double (*pd2) (double);
extern double (*pd6) (double, float);
extern double (*pd10) (double, double);

extern _Complex float (*pcf0) (void);
extern _Complex float (*pcf1) (float);
extern _Complex float (*pcf5) (float, float);
extern _Complex float (*pcf9) (float, double);
extern _Complex float (*pcf2) (double);
extern _Complex float (*pcf6) (double, float);
extern _Complex float (*pcf10) (double, double);

extern _Complex double (*pcd0) (void);
extern _Complex double (*pcd1) (float);
extern _Complex double (*pcd5) (float, float);
extern _Complex double (*pcd9) (float, double);
extern _Complex double (*pcd2) (double);
extern _Complex double (*pcd6) (double, float);
extern _Complex double (*pcd10) (double, double);

/* Macros for results checking.  */
#define CHECK_RESULT(x, y) if ((x) != (y)) abort ()
#define CHECK_VOID_RESULT(x, y)  CHECK_RESULT (((x), the_result), y)

/* Call functions through pointers and check against expected results.  */
void
test (void)
{

  CHECK_VOID_RESULT (v0 (), 1.0);
  CHECK_VOID_RESULT (v1 (1.0), 2.0);
  CHECK_VOID_RESULT (v5 (5.0, 6.0), 12.0);
  CHECK_VOID_RESULT (v9 (9.0, 10.0), 20.0);
  CHECK_VOID_RESULT (v2 (2.0), 3.0);
  CHECK_VOID_RESULT (v6 (6.0, 7.0), 14.0);
  CHECK_VOID_RESULT (v10 (10.0, 11.0), 22.0);

  CHECK_RESULT (f0 (), 1.0);
  CHECK_RESULT (f1 (1.0), 2.0);
  CHECK_RESULT (f5 (5.0, 6.0), 12.0);
  CHECK_RESULT (f9 (9.0, 10.0), 20.0);
  CHECK_RESULT (f2 (2.0), 3.0);
  CHECK_RESULT (f6 (6.0, 7.0), 14.0);
  CHECK_RESULT (f10 (10.0, 11.0), 22.0);

  CHECK_RESULT (d0 (), 1.0);
  CHECK_RESULT (d1 (1.0), 2.0);
  CHECK_RESULT (d5 (5.0, 6.0), 12.0);
  CHECK_RESULT (d9 (9.0, 10.0), 20.0);
  CHECK_RESULT (d2 (2.0), 3.0);
  CHECK_RESULT (d6 (6.0, 7.0), 14.0);
  CHECK_RESULT (d10 (10.0, 11.0), 22.0);

  CHECK_RESULT (cf0 (), 1.0 + 0.0i);
  CHECK_RESULT (cf1 (1.0), 2.0 + 1.0i);
  CHECK_RESULT (cf5 (5.0, 6.0), 12.0 + 5.0i);
  CHECK_RESULT (cf9 (9.0, 10.0), 20.0 + 9.0i);
  CHECK_RESULT (cf2 (2.0), 3.0 + 2.0i);
  CHECK_RESULT (cf6 (6.0, 7.0), 14.0 + 6.0i);
  CHECK_RESULT (cf10 (10.0, 11.0), 22.0 + 10.0i);

  CHECK_RESULT (cd0 (), 1.0 + 0.0i);
  CHECK_RESULT (cd1 (1.0), 2.0 + 1.0i);
  CHECK_RESULT (cd5 (5.0, 6.0), 12.0 + 5.0i);
  CHECK_RESULT (cd9 (9.0, 10.0), 20.0 + 9.0i);
  CHECK_RESULT (cd2 (2.0), 3.0 + 2.0i);
  CHECK_RESULT (cd6 (6.0, 7.0), 14.0 + 6.0i);
  CHECK_RESULT (cd10 (10.0, 11.0), 22.0 + 10.0i);

  CHECK_VOID_RESULT ((*pv0) (), 1.0);
  CHECK_VOID_RESULT ((*pv1) (1.0), 2.0);
  CHECK_VOID_RESULT ((*pv5) (5.0, 6.0), 12.0);
  CHECK_VOID_RESULT ((*pv9) (9.0, 10.0), 20.0);
  CHECK_VOID_RESULT ((*pv2) (2.0), 3.0);
  CHECK_VOID_RESULT ((*pv6) (6.0, 7.0), 14.0);
  CHECK_VOID_RESULT ((*pv10) (10.0, 11.0), 22.0);

  CHECK_RESULT ((*pf0) (), 1.0);
  CHECK_RESULT ((*pf1) (1.0), 2.0);
  CHECK_RESULT ((*pf5) (5.0, 6.0), 12.0);
  CHECK_RESULT ((*pf9) (9.0, 10.0), 20.0);
  CHECK_RESULT ((*pf2) (2.0), 3.0);
  CHECK_RESULT ((*pf6) (6.0, 7.0), 14.0);
  CHECK_RESULT ((*pf10) (10.0, 11.0), 22.0);

  CHECK_RESULT ((*pd0) (), 1.0);
  CHECK_RESULT ((*pd1) (1.0), 2.0);
  CHECK_RESULT ((*pd5) (5.0, 6.0), 12.0);
  CHECK_RESULT ((*pd9) (9.0, 10.0), 20.0);
  CHECK_RESULT ((*pd2) (2.0), 3.0);
  CHECK_RESULT ((*pd6) (6.0, 7.0), 14.0);
  CHECK_RESULT ((*pd10) (10.0, 11.0), 22.0);

  CHECK_RESULT ((*pcf0) (), 1.0 + 0.0i);
  CHECK_RESULT ((*pcf1) (1.0), 2.0 + 1.0i);
  CHECK_RESULT ((*pcf5) (5.0, 6.0), 12.0 + 5.0i);
  CHECK_RESULT ((*pcf9) (9.0, 10.0), 20.0 + 9.0i);
  CHECK_RESULT ((*pcf2) (2.0), 3.0 + 2.0i);
  CHECK_RESULT ((*pcf6) (6.0, 7.0), 14.0 + 6.0i);
  CHECK_RESULT ((*pcf10) (10.0, 11.0), 22.0 + 10.0i);

  CHECK_RESULT ((*pcd0) (), 1.0 + 0.0i);
  CHECK_RESULT ((*pcd1) (1.0), 2.0 + 1.0i);
  CHECK_RESULT ((*pcd5) (5.0, 6.0), 12.0 + 5.0i);
  CHECK_RESULT ((*pcd9) (9.0, 10.0), 20.0 + 9.0i);
  CHECK_RESULT ((*pcd2) (2.0), 3.0 + 2.0i);
  CHECK_RESULT ((*pcd6) (6.0, 7.0), 14.0 + 6.0i);
  CHECK_RESULT ((*pcd10) (10.0, 11.0), 22.0 + 10.0i);
}
