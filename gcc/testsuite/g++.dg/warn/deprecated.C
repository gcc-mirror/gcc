/* Test __attribute__ ((deprecated)) */
/* { dg-do compile } */
/* { dg-options "-Wdeprecated-declarations -fmessage-length=0" } */

#if 1
typedef int INT1 __attribute__((deprecated));
typedef INT1 INT2 __attribute__ ((__deprecated__));

typedef INT1 INT1a; 			/* { dg-warning "'INT1' is deprecated" "" } */
typedef INT1 INT1b __attribute__ ((deprecated));

INT1 should_be_unavailable; 		/* { dg-warning "'INT1' is deprecated" "" } */
INT1a should_not_be_deprecated;

INT1 f1(void) __attribute__ ((deprecated)); 
INT1 f2(void) { return 0; }		/* { dg-warning "'INT1' is deprecated" "" } */

INT2 f3(void) __attribute__ ((__deprecated__)); 
INT2 f4(void) { return 0; }		/* { dg-warning "'INT2' is deprecated" "" } */
int f5(INT2 x);				/* { dg-warning "'INT2' is deprecated" "" } */
int f6(INT2 x) __attribute__ ((__deprecated__));

typedef enum Color {red, green, blue} Color __attribute__((deprecated));

int g1;
int g2 __attribute__ ((deprecated));
int g3 __attribute__ ((__deprecated__));
Color k;				/* { dg-warning "'Color' is deprecated" "" } */

typedef struct {
  int field1;
  int field2 __attribute__ ((deprecated));
  int field3;
  int field4 __attribute__ ((__deprecated__));
  union {
    int field5;
    int field6 __attribute__ ((deprecated));
  } u1;
  int field7:1;
  int field8:1 __attribute__ ((deprecated));
  union {
    int field9;
    int field10;
  } u2 __attribute__ ((deprecated));
} S1;

int func1()
{
   INT1 w;				/* { dg-warning "'INT1' is deprecated" "" } */
   int x __attribute__ ((deprecated));
   int y __attribute__ ((__deprecated__));
   int z;
   int (*pf)() = f1;			/* { dg-warning "'INT1 f1\\(\\)' is deprecated" "" } */

   z = w + x + y + g1 + g2 + g3;	/* { dg-warning "'x' is deprecated" "" } */
   					/* { dg-warning "'y' is deprecated" "y" { target *-*-* } 55 } */
   					/* { dg-warning "'g2' is deprecated" "g2" { target *-*-* } 55 } */
   					/* { dg-warning "'g3' is deprecated" "g3" { target *-*-* } 55 } */
   return f1(); 			/* { dg-warning "'INT1 f1\\(\\)' is deprecated" "f1" } */
}

int func2(S1 *p)
{
  S1 lp;
  
  if (p->field1)
     return p->field2;			/* { dg-warning "'S1::field2' is deprecated" "" } */
  else if (lp.field4)			/* { dg-warning "'S1::field4' is deprecated" "" } */
     return p->field3;
  
  p->u1.field5 = g1 + p->field7;
  p->u2.field9;				/* { dg-warning "'S1::u2' is deprecated" "" } */
  return p->u1.field6 + p->field8;	/* { dg-warning "'S1::<unnamed union>::field6' is deprecated" "" } */
  					/* { dg-warning "'S1::field8' is deprecated" "field8" { target *-*-* } 73 } */
}

struct SS1 {
  int x;
  INT1 y; 				/* { dg-warning "'INT1' is deprecated" "" } */
} __attribute__ ((deprecated));

struct SS1 *p1;				/* { dg-warning "'SS1' is deprecated" "" } */

struct __attribute__ ((__deprecated__)) SS2 {
  int x;
  INT1 y; 				/* { dg-warning "'INT1' is deprecated" "" } */
};

struct SS2 *p2;				/* { dg-warning "'SS2' is deprecated" "" } */
#endif

#ifdef __cplusplus
class T {
  public:
    void member1(int) __attribute__ ((deprecated));
    void member2(INT1) __attribute__ ((__deprecated__)); /* { dg-warning "'INT1' is deprecated" "" } */
    int member3(T *);
    int x;
} __attribute__ ((deprecated));

T *p3;				// { dg-warning "'T' is deprecated" }

inline void T::member1(int) {}

int T::member3(T *p)		// { dg-warning "'T' is deprecated" }
{
  p->member1(1);			/* { dg-warning "'void T::member1\\(int\\)' is deprecated" "" } */
  (*p).member1(2);			/* { dg-warning "'void T::member1\\(int\\)' is deprecated" "" } */
  p->member2(1);			/* { dg-warning "'void T::member2\\(INT1\\)' is deprecated" "" } */
  (*p).member2(2);			/* { dg-warning "'void T::member2\\(INT1\\)' is deprecated" "" } */
  p->member3(p);
  (*p).member3(p);
  return f1(); 				/* { dg-warning "'INT1 f1\\(\\)' is deprecated" "" } */
}
#endif


