/* Test __attribute__ ((unavailable("message"))) */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int INT1 __attribute__((unavailable("You can't use INT1")));
typedef INT1 INT2 __attribute__ ((__unavailable__("You can't use INT2")));

typedef INT1 INT1a; 			/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */
typedef INT1 INT1b __attribute__ ((unavailable("You can't use INT1b")));

INT1 should_be_unavailable; 		/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */
INT1a should_not_be_unavailable;

INT1 f1(void) __attribute__ ((unavailable("You can't use f1")));
INT1 f2(void) { return 0; }		/* { dg-error "'INT1' is unavailable: You can't use INT1" "" } */

INT2 f3(void) __attribute__ ((__unavailable__("You can't use f3")));
INT2 f4(void) { return 0; }		/* { dg-error "'INT2' is unavailable: You can't use INT2" "" } */
int f5(INT2 x);				/* { dg-error "'INT2' is unavailable: You can't use INT2" "" } */
int f6(INT2 x) __attribute__ ((__unavailable__("You can't use f6"))); /* { dg-error "'INT2' is unavailable: You can't use INT2" "" } */

typedef enum {red, green, blue} Color __attribute__((unavailable("You can't use Color")));

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
   int x __attribute__ ((unavailable("Avoid x")));
   int y __attribute__ ((__unavailable__("Bad y")));
   int z;
   int (*pf)() = f1;			/* { dg-error "'f1' is unavailable: You can't use f1" "" } */

   z = w + x + y + g1 + g2 + g3;	/* { dg-error "'x' is unavailable: Avoid x" "" } */
					/* { dg-error "'y' is unavailable: Bad y" "y" { target *-*-* } .-1  } */
					/* { dg-error "'g2' is unavailable: You can't use g2" "g2" { target *-*-* } .-2  }  */
					/* { dg-error "'g3' is unavailable: You can't use g3" "g3" { target *-*-* } .-3  } */
   return f1(); 			/* { dg-error "'f1' is unavailable: You can't use f1" "" } */
}

int func2(S1 *p)
{
  S1 lp;

  if (p->field1)
     return p->field2;			/* { dg-error "'field2' is unavailable: You can't use field2" "" } */
  else if (lp.field4)			/* { dg-error "'field4' is unavailable: You can't use field4" "" } */
     return p->field3;

  p->u1.field5 = g1 + p->field7;
  p->u2.field9;				/* { dg-error "'u2' is unavailable: You can't use u2" "" } */
  return p->u1.field6 + p->field8;	/* { dg-error "'field6' is unavailable: You can't use field6" "" } */
					/* { dg-error "'field8' is unavailable: You can't use field8" "field8" { target *-*-* } .-1 } */
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
