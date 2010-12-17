// PR c++/46401
// { dg-do compile }
// { dg-options "-Wsequence-point" }

struct S
{
  S ();
  S &operator<< (const char *);
  S (const S &);
};

#define N1(n) << #n
#define N2(n) N1(n)
#define N3(n) N2(n##0) N2(n##1) N2(n##2) N2(n##3) N2(n##4) \
	      N2(n##5) N2(n##6) N2(n##7) N2(n##8) N2(n##9)
#define N4(n) N3(n##0) N3(n##1) N3(n##2) N3(n##3) N3(n##4) \
	      N3(n##5) N3(n##6) N3(n##7) N3(n##8) N3(n##9)
#define N5(n) N4(n##0) N4(n##1) N4(n##2) N4(n##3) N4(n##4) \
	      N4(n##5) N4(n##6) N4(n##7) N4(n##8) N4(n##9)
S s = S () N5(a) N5(b);
