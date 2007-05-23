/* All test functions return the sum of arguments, plus 1.
   Void-returning functions put the result in the_result.
   Complex-returning functions return their signature number as the
   (constant) imaginary part of the result.  */

double the_result;

void v0 (void) 			{ the_result = 1.0; }
void v1 (float x)		{ the_result = 1.0 + x; }
void v5 (float x, float y) 	{ the_result = 1.0 + x + y; }
void v9 (float x, double y) 	{ the_result = 1.0 + x + y; }
void v2 (double x)		{ the_result = 1.0 + x; }
void v6 (double x, float y)	{ the_result = 1.0 + x + y; }
void v10 (double x, double y)  	{ the_result = 1.0 + x + y; }

float f0 (void) 		{ return 1.0; }
float f1 (float x)		{ return 1.0 + x; }
float f5 (float x, float y) 	{ return 1.0 + x + y; }
float f9 (float x, double y) 	{ return 1.0 + x + y; }
float f2 (double x)		{ return 1.0 + x; }
float f6 (double x, float y)	{ return 1.0 + x + y; }
float f10 (double x, double y)	{ return 1.0 + x + y; }

double d0 (void) 		{ return 1.0; }
double d1 (float x)		{ return 1.0 + x; }
double d5 (float x, float y) 	{ return 1.0 + x + y; }
double d9 (float x, double y) 	{ return 1.0 + x + y; }
double d2 (double x)		{ return 1.0 + x; }
double d6 (double x, float y)	{ return 1.0 + x + y; }
double d10 (double x, double y) { return 1.0 + x + y; }

_Complex float cf0 (void) 			{ return 1.0 + 0.0i; }
_Complex float cf1 (float x)			{ return 1.0 + x + 1.0i; }
_Complex float cf5 (float x, float y) 		{ return 1.0 + x + y + 5.0i; }
_Complex float cf9 (float x, double y) 		{ return 1.0 + x + y + 9.0i; }
_Complex float cf2 (double x)			{ return 1.0 + x + 2.0i; }
_Complex float cf6 (double x, float y)		{ return 1.0 + x + y + 6.0i; }
_Complex float cf10 (double x, double y)	{ return 1.0 + x + y + 10.0i; }

_Complex double cd0 (void) 			{ return 1.0 + 0.0i; }
_Complex double cd1 (float x)			{ return 1.0 + x + 1.0i; }
_Complex double cd5 (float x, float y) 		{ return 1.0 + x + y + 5.0i; }
_Complex double cd9 (float x, double y) 	{ return 1.0 + x + y + 9.0i; }
_Complex double cd2 (double x)			{ return 1.0 + x + 2.0i; }
_Complex double cd6 (double x, float y)		{ return 1.0 + x + y + 6.0i; }
_Complex double cd10 (double x, double y)	{ return 1.0 + x + y + 10.0i; }


/* Declare and initialize all the pointer-to-function variables.  */

void (*pv0) (void);
void (*pv1) (float);
void (*pv5) (float, float);
void (*pv9) (float, double);
void (*pv2) (double);
void (*pv6) (double, float);
void (*pv10) (double, double);

float (*pf0) (void);
float (*pf1) (float);
float (*pf5) (float, float);
float (*pf9) (float, double);
float (*pf2) (double);
float (*pf6) (double, float);
float (*pf10) (double, double);

double (*pd0) (void);
double (*pd1) (float);
double (*pd5) (float, float);
double (*pd9) (float, double);
double (*pd2) (double);
double (*pd6) (double, float);
double (*pd10) (double, double);

_Complex float (*pcf0) (void);
_Complex float (*pcf1) (float);
_Complex float (*pcf5) (float, float);
_Complex float (*pcf9) (float, double);
_Complex float (*pcf2) (double);
_Complex float (*pcf6) (double, float);
_Complex float (*pcf10) (double, double);

_Complex double (*pcd0) (void);
_Complex double (*pcd1) (float);
_Complex double (*pcd5) (float, float);
_Complex double (*pcd9) (float, double);
_Complex double (*pcd2) (double);
_Complex double (*pcd6) (double, float);
_Complex double (*pcd10) (double, double);

void
init (void)
{
  pv0 = v0;
  pv1 = v1;
  pv5 = v5;
  pv9 = v9;
  pv2 = v2;
  pv6 = v6;
  pv10 = v10;

  pf0 = f0;
  pf1 = f1;
  pf5 = f5;
  pf9 = f9;
  pf2 = f2;
  pf6 = f6;
  pf10 = f10;

  pd0 = d0;
  pd1 = d1;
  pd5 = d5;
  pd9 = d9;
  pd2 = d2;
  pd6 = d6;
  pd10 = d10;

  pcf0 = cf0;
  pcf1 = cf1;
  pcf5 = cf5;
  pcf9 = cf9;
  pcf2 = cf2;
  pcf6 = cf6;
  pcf10 = cf10;

  pcd0 = cd0;
  pcd1 = cd1;
  pcd5 = cd5;
  pcd9 = cd9;
  pcd2 = cd2;
  pcd6 = cd6;
  pcd10 = cd10;
}
