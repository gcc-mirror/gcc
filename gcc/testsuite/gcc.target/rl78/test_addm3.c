/* Remove `-ansi' from options to enable the use of __far and long long.  */
/* { dg-options "" } */

#define ADD(TYPE, name)				\
  TYPE						\
  add##name(TYPE a, TYPE b)			\
  {						\
    return a + b;				\
  }						\
  
#define ADDIMM(TYPE, name)			\
  TYPE						\
  addimm##name(TYPE a)				\
  {						\
    return a + 50;				\
  }						\

#define ADDFAR(TYPE, name)			\
  TYPE __far gf##name;				\
  void						\
  addfar##name(TYPE __far *pa, TYPE b)		\
  {						\
    gf##name += b;				\
    *pa += 50;					\
  }						\
  

ADD (char, qi3)
ADD (int, hi3)
ADD (long, si3)
ADD (long long, di3)
ADD (float, sf3)
ADD (double, df3)

ADDIMM (char, qi3)
ADDIMM (int, hi3)
ADDIMM (long, si3)
ADDIMM (long long, di3)
ADDIMM (float, sf3)
ADDIMM (double, df3)

ADDFAR (char, qi3)
ADDFAR (int, hi3)
ADDFAR (long, si3)
ADDFAR (long long, di3)
ADDFAR (float, sf3)
ADDFAR (double, df3)

char aqi1, aqi2;
int ahi1, ahi2;
long asi1, asi2;
long long adi1, adi2;
float af1, af2;
double ad1, ad2;

void
testglobal (void)
{
  aqi1 += aqi2;
  ahi1 += ahi2;
  asi1 += asi2;
  adi1 += adi2;
  af1 += af2;
  ad1 += ad2;
}

void
testglobal2 (void)
{
  aqi1 += 10;
  ahi1 += 11;
  asi1 += 12;
  adi1 += 13;
  af1 += 2.0;
  ad1 += 4.0;
}

void
testptr (char *aqi1, int *ahi1, long *asi1, long long *adi1, float *af1, double *ad1, 
	 char *aqi2, int *ahi2, long *asi2, long long *adi2, float *af2, double *ad2)
{
  *aqi1 += *aqi2;
  *ahi1 += *ahi2;
  *asi1 += *asi2;
  *adi1 += *adi2;
  *af1 += *af2;
  *ad1 += *ad2;
}

void
testptr2 (char *aqi1, int *ahi1, long *asi1, long long *adi1, float *af1, double *ad1)
{
  *aqi1 += 5;
  *ahi1 += 10;
  *asi1 += 11;
  *adi1 += 12;
  *af1 += 4.5;
  *ad1 += 5.5;
}
