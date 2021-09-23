/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int INT1 __attribute__((unavailable));
typedef INT1 INT2 __attribute__ ((__unavailable__));

typedef INT1 INT1a; 			/* { dg-error "'INT1' is unavailable" "" } */
typedef INT1 INT1b __attribute__ ((unavailable));

INT1 should_be_unavailable; 		/* { dg-error "'INT1' is unavailable" "" } */
INT1a should_not_be_unavailable;

INT1 f1(void) __attribute__ ((unavailable));
INT1 f2(void) { return 0; }		/* { dg-error "'INT1' is unavailable" "" } */

INT2 f3(void) __attribute__ ((__unavailable__));
INT2 f4(void) { return 0; }		/* { dg-error "'INT2' is unavailable" "" } */
int f5(INT2 x);				/* { dg-error "'INT2' is unavailable" "" } */
int f6(INT2 x) __attribute__ ((__unavailable__));

typedef enum Color {red, green, blue} Color __attribute__((unavailable));

int g1;
int g2 __attribute__ ((unavailable));
int g3 __attribute__ ((__unavailable__));
Color k;				/* { dg-error "'Color' is unavailable" "" } */

typedef struct {
  int field1;
  int field2 __attribute__ ((unavailable));
  int field3;
  int field4 __attribute__ ((__unavailable__));
  union {
    int field5;
    int field6 __attribute__ ((unavailable));
  } u1;
  int field7:1;
  int field8:1 __attribute__ ((unavailable));
  union {
    int field9;
    int field10;
  } u2 __attribute__ ((unavailable));
} S1;

int func1()
{
   INT1 w;				/* { dg-error "'INT1' is unavailable" "" } */
   int x __attribute__ ((unavailable));
   int y __attribute__ ((__unavailable__));
   int z;
   int (*pf)() = f1;			/* { dg-error "'INT1 f1\\(\\)' is unavailable" "" } */

   z = w + x + y + g1 + g2 + g3;	/* { dg-error "'x' is unavailable" "" } */
					/* { dg-error "'y' is unavailable" "y" { target *-*-* } .-1 } */
					/* { dg-error "'g2' is unavailable" "g2" { target *-*-* } .-2 } */
					/* { dg-error "'g3' is unavailable" "g3" { target *-*-* } .-3 } */
   return f1(); 			/* { dg-error "'INT1 f1\\(\\)' is unavailable" "f1" } */
}

int func2(S1 *p)
{
  S1 lp;

  if (p->field1)
     return p->field2;			/* { dg-error "'S1::field2' is unavailable" "" } */
  else if (lp.field4)			/* { dg-error "'S1::field4' is unavailable" "" } */
     return p->field3;

  p->u1.field5 = g1 + p->field7;
  p->u2.field9;				/* { dg-error "'S1::u2' is unavailable" "" } */
  return p->u1.field6 + p->field8;	/* { dg-error "'S1::<unnamed union>::field6' is unavailable" "" } */
					/* { dg-error "'S1::field8' is unavailable" "field8" { target *-*-* } .-1 } */
}

struct SS1 {
  int x;
  INT1 y; 				/* { dg-error "'INT1' is unavailable" "" } */
} __attribute__ ((unavailable));

struct SS1 *p1;				/* { dg-error "'SS1' is unavailable" "" } */

struct __attribute__ ((__unavailable__)) SS2 {
  int x;
  INT1 y; 				/* { dg-error "'INT1' is unavailable" "" } */
};

struct SS2 *p2;				/* { dg-error "'SS2' is unavailable" "" } */

#ifdef __cplusplus
class T {
  public:
    void member1(int) __attribute__ ((unavailable));
    void member2(INT1) __attribute__ ((__unavailable__));
    int member3(T *);
    int x;
} __attribute__ ((unavailable));

T *p3;				// { dg-error "'T' is unavailable" }

inline void T::member1(int) {}

int T::member3(T *p)		// { dg-error "'T' is unavailable" }
{
  p->member1(1);			/* { dg-error "'void T::member1\\(int\\)' is unavailable" "" } */
  (*p).member1(2);			/* { dg-error "'void T::member1\\(int\\)' is unavailable" "" } */
  p->member2(1);			/* { dg-error "'void T::member2\\(INT1\\)' is unavailable" "" } */
  (*p).member2(2);			/* { dg-error "'void T::member2\\(INT1\\)' is unavailable" "" } */
  p->member3(p);
  (*p).member3(p);
  return f1(); 				/* { dg-error "'INT1 f1\\(\\)' is unavailable" "" } */
}
#endif
