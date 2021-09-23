/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int INT1 __attribute__((unavailable("You can't use INT1")));
typedef INT1 INT2 __attribute__ ((__unavailable__("You can't use INT2")));

typedef INT1 INT1a; 			/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */

INT1 should_be_unavailable; 		/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */
INT1a should_not_be_unavailable;

INT1 f1(void) __attribute__ ((unavailable("You can't use f1")));
INT1 f2(void) { return 0; }		/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */

INT2 f3(void) __attribute__ ((__unavailable__("You can't use f3")));
INT2 f4(void) { return 0; }		/* { dg-error "'INT2' is unavailable: You can't use INT2" "" } */
int f5(INT2 x);				/* { dg-error "'INT2' is unavailable" "" } */
int f6(INT2 x) __attribute__ ((__unavailable__("You can't use f6")));

typedef enum Color {red, green, blue} Color __attribute__((unavailable("You can't use Color")));

int g1;
int g2 __attribute__ ((unavailable("You can't use g2")));
int g3 __attribute__ ((__unavailable__("You can't use g3")));
Color k;				/* { dg-error "'Color' is unavailable: You can't use Color" "" } */

typedef struct {
  int field1;
  int field2 __attribute__ ((unavailable("You can't use field2")));
  int field3;
  int field4 __attribute__ ((__unavailable__("You can't use field4")));
  union {
    int field5;
    int field6 __attribute__ ((unavailable("You can't use field6")));
  } u1;
  int field7:1;
  int field8:1 __attribute__ ((unavailable("You can't use field8")));
  union {
    int field9;
    int field10;
  } u2 __attribute__ ((unavailable("You can't use u2")));
} S1;

int func1()
{
   INT1 w;				/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */
   int x __attribute__ ((unavailable("You can't use x")));
   int y __attribute__ ((__unavailable__("You can't use y")));
   int z;
   int (*pf)() = f1;			/* { dg-error "'INT1 f1\\(\\)' is unavailable: You can't use f1" "" } */

   z = w + x + y + g1 + g2 + g3;	/* { dg-error "'x' is unavailable: You can't use x" "" } */
					/* { dg-error "'y' is unavailable: You can't use y" "y" { target *-*-* } .-1 } */
					/* { dg-error "'g2' is unavailable: You can't use g2" "g2" { target *-*-* } .-2 } */
					/* { dg-error "'g3' is unavailable: You can't use g3" "g3" { target *-*-* } .-3 } */
   return f1(); 			/* { dg-error "'INT1 f1\\(\\)' is unavailable: You can't use f1" "f1" } */
}

int func2(S1 *p)
{
  S1 lp;

  if (p->field1)
     return p->field2;			/* { dg-error "'S1::field2' is unavailable: You can't use field2" "" } */
  else if (lp.field4)			/* { dg-error "'S1::field4' is unavailable: You can't use field4" "" } */
     return p->field3;

  p->u1.field5 = g1 + p->field7;
  p->u2.field9;				/* { dg-error "'S1::u2' is unavailable: You can't use u2" "" } */
  return p->u1.field6 + p->field8;	/* { dg-error "'S1::<unnamed union>::field6' is unavailable: You can't use field6" "" } */
					/* { dg-error "'S1::field8' is unavailable: You can't use field8" "field8" { target *-*-* } .-1 } */
}

struct SS1 {
  int x;
  INT1 y; 				/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */
} __attribute__ ((unavailable("You can't use SS1")));

struct SS1 *p1;				/* { dg-error "'SS1' is unavailable: You can't use SS1" "" } */

struct __attribute__ ((__unavailable__("You can't use SS2"))) SS2 {
  int x;
  INT1 y; 				/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */
};

struct SS2 *p2;				/* { dg-error "'SS2' is unavailable: You can't use SS2" "" } */

class T {
  public:
    void member1(int) __attribute__ ((unavailable("You can't use member1")));
    void member2(INT1) __attribute__ ((__unavailable__("You can't use member2")));
    int member3(T *);
    int x;
} __attribute__ ((unavailable("You can't use T")));

T *p3;				// { dg-error "'T' is unavailable: You can't use T" }

inline void T::member1(int) {}

int T::member3(T *p)		// { dg-error "'T' is unavailable: You can't use T" }
{
  p->member1(1);			/* { dg-error "'void T::member1\\(int\\)' is unavailable: You can't use member1" "" } */
  (*p).member1(2);			/* { dg-error "'void T::member1\\(int\\)' is unavailable: You can't use member1" "" } */
  p->member2(1);			/* { dg-error "'void T::member2\\(INT1\\)' is unavailable: You can't use member2" "" } */
  (*p).member2(2);			/* { dg-error "'void T::member2\\(INT1\\)' is unavailable: You can't use member2" "" } */
  p->member3(p);
  (*p).member3(p);
  return f1(); 				/* { dg-error "'INT1 f1\\(\\)' is unavailable: You can't use f1" "" } */
}
