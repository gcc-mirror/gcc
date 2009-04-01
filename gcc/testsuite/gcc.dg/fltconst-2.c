/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* Check that d or D is recognized as a float constant suffix.  */

double d = 0.5d;
double D = 0.5D;

/* Check that d or D is recognized in a hexadecimal floating constant.  */

double hd1 = 0x1.8p1d;
double hd2 = 0x1.p+1D;
double hd3 = 0x0.8p-1d;

/* Check that imaginary constant suffixes are still recognized with
   only i, I, j, or J.  */

double i = 0.5i;
double I = 0.5I;
double j = 0.5j;
double J = 0.5J;

/* Check that imaginary constant suffixes are allowed with d or D.  */

double di = 0.5di;
double dI = 0.5dI;
double Di = 0.5Di;
double DI = 0.5DI;
double dj = 0.5dj;
double dJ = 0.5dJ;
double Dj = 0.5Dj;
double DJ = 0.5DJ;
double id = 0.5id;
double iD = 0.5iD;
double Id = 0.5Id;
double ID = 0.5ID;
double jd = 0.5jd;
double jD = 0.5jD;
double Jd = 0.5Jd;
double JD = 0.5JD;
