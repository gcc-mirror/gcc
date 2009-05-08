/* Test __attribute__ ((deprecated("message"))) */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations -fmessage-length=0" } */

typedef int INT1 __attribute__((deprecated("Please avoid INT1")));
typedef INT1 INT2 __attribute__ ((__deprecated__("Please avoid INT2")));

typedef INT1 INT1a; 			/* { dg-warning "'INT1' is deprecated .declared at \[^\n\]*: Please avoid INT1" "" } */

INT1 should_be_unavailable; 		/* { dg-warning "'INT1' is deprecated .declared at \[^\n\]*: Please avoid INT1" "" } */
INT1a should_not_be_deprecated;

INT1 f1(void) __attribute__ ((deprecated("Please avoid f1"))); 
INT1 f2(void) { return 0; }		/* { dg-warning "'INT1' is deprecated .declared at \[^\n\]*: Please avoid INT1" "" } */

INT2 f3(void) __attribute__ ((__deprecated__("Please avoid f3"))); 
INT2 f4(void) { return 0; }		/* { dg-warning "'INT2' is deprecated .declared at \[^\n\]*: Please avoid INT2" "" } */
int f5(INT2 x);				/* { dg-warning "'INT2' is deprecated" "" } */
int f6(INT2 x) __attribute__ ((__deprecated__("Please avoid f6")));

typedef enum Color {red, green, blue} Color __attribute__((deprecated("Please avoid Color")));

int g1;
int g2 __attribute__ ((deprecated("Please avoid g2")));
int g3 __attribute__ ((__deprecated__("Please avoid g3")));
Color k;				/* { dg-warning "'Color' is deprecated .declared at \[^\n\]*: Please avoid Color" "" } */

typedef struct {
  int field1;
  int field2 __attribute__ ((deprecated("Please avoid field2")));
  int field3;
  int field4 __attribute__ ((__deprecated__("Please avoid field4")));
  union {
    int field5;
    int field6 __attribute__ ((deprecated("Please avoid field6")));
  } u1;
  int field7:1;
  int field8:1 __attribute__ ((deprecated("Please avoid field8")));
  union {
    int field9;
    int field10;
  } u2 __attribute__ ((deprecated("Please avoid u2")));
} S1;

int func1()
{
   INT1 w;				/* { dg-warning "'INT1' is deprecated .declared at \[^\n\]*: Please avoid INT1" "" } */
   int x __attribute__ ((deprecated("Please avoid x")));
   int y __attribute__ ((__deprecated__("Please avoid y")));
   int z;
   int (*pf)() = f1;			/* { dg-warning "'INT1 f1\\(\\)' is deprecated .declared at \[^\n\]*: Please avoid f1" "" } */

   z = w + x + y + g1 + g2 + g3;	/* { dg-warning "'x' is deprecated .declared at \[^\n\]*: Please avoid x" "" } */
   					/* { dg-warning "'y' is deprecated .declared at \[^\n\]*: Please avoid y" "y" { target *-*-* } 53 } */
   					/* { dg-warning "'g2' is deprecated .declared at \[^\n\]*: Please avoid g2" "g2" { target *-*-* } 53 } */
   					/* { dg-warning "'g3' is deprecated .declared at \[^\n\]*: Please avoid g3" "g3" { target *-*-* } 53 } */
   return f1(); 			/* { dg-warning "'INT1 f1\\(\\)' is deprecated .declared at \[^\n\]*: Please avoid f1" "f1" } */
}

int func2(S1 *p)
{
  S1 lp;
  
  if (p->field1)
     return p->field2;			/* { dg-warning "'S1::field2' is deprecated .declared at \[^\n\]*: Please avoid field2" "" } */
  else if (lp.field4)			/* { dg-warning "'S1::field4' is deprecated .declared at \[^\n\]*: Please avoid field4" "" } */
     return p->field3;
  
  p->u1.field5 = g1 + p->field7;
  p->u2.field9;				/* { dg-warning "'S1::u2' is deprecated .declared at \[^\n\]*: Please avoid u2" "" } */
  return p->u1.field6 + p->field8;	/* { dg-warning "'S1::<anonymous union>::field6' is deprecated .declared at \[^\n\]*: Please avoid field6" "" } */
  					/* { dg-warning "'S1::field8' is deprecated .declared at \[^\n\]*: Please avoid field8" "field8" { target *-*-* } 71 } */
}

struct SS1 {
  int x;
  INT1 y; 				/* { dg-warning "'INT1' is deprecated .declared at \[^\n\]*: Please avoid INT1" "" } */
} __attribute__ ((deprecated("Please avoid SS1")));

struct SS1 *p1;				/* { dg-warning "'SS1' is deprecated .declared at \[^\n\]*: Please avoid SS1" "" } */

struct __attribute__ ((__deprecated__("Please avoid SS2"))) SS2 {
  int x;
  INT1 y; 				/* { dg-warning "'INT1' is deprecated .declared at \[^\n\]*: Please avoid INT1" "" } */
};

struct SS2 *p2;				/* { dg-warning "'SS2' is deprecated .declared at \[^\n\]*: Please avoid SS2" "" } */

class T {
  public:
    void member1(int) __attribute__ ((deprecated("Please avoid member1")));
    void member2(INT1) __attribute__ ((__deprecated__("Please avoid member2"))); /* { dg-warning "'INT1' is deprecated" "" } */
    int member3(T *);
    int x;
} __attribute__ ((deprecated("Please avoid T")));

T *p3;				// { dg-warning "'T' is deprecated .declared at \[^\n\]*: Please avoid T" }

inline void T::member1(int) {}

int T::member3(T *p)		// { dg-warning "'T' is deprecated .declared at \[^\n\]*: Please avoid T" }
{
  p->member1(1);			/* { dg-warning "'void T::member1\\(int\\)' is deprecated .declared at \[^\n\]*: Please avoid member1" "" } */
  (*p).member1(2);			/* { dg-warning "'void T::member1\\(int\\)' is deprecated .declared at \[^\n\]*: Please avoid member1" "" } */
  p->member2(1);			/* { dg-warning "'void T::member2\\(INT1\\)' is deprecated .declared at \[^\n\]*: Please avoid member2" "" } */
  (*p).member2(2);			/* { dg-warning "'void T::member2\\(INT1\\)' is deprecated .declared at \[^\n\]*: Please avoid member2" "" } */
  p->member3(p);
  (*p).member3(p);
  return f1(); 				/* { dg-warning "'INT1 f1\\(\\)' is deprecated .declared at \[^\n\]*: Please avoid f1" "" } */
}
