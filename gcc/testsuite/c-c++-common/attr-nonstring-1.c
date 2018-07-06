/* Test to exercise attribute "nonstring" syntax.
   { dg-do compile }
   { dg-options "-Wattributes" }  */

#define ATTR(list) __attribute__ (list)
#define NONSTR     ATTR ((nonstring))

/* Verify it's accepted on char arrays.  */
extern NONSTR char nsx_1[];
extern char NONSTR nsx_2[];
extern char nsx_3[] NONSTR;

extern NONSTR char ns1[1];
extern char NONSTR ns3[3];
extern char ns5[5] NONSTR;

/* Verify it's accepted on char pointers.  */
extern NONSTR char* pns_1;
extern char NONSTR* pns_2;
extern char* NONSTR pns_3;

struct S
{
/* Verify it's accepted on char member pointers.  */
  NONSTR char* mpns_1;
  char NONSTR* mpns_2;
  char* NONSTR mpns_3;

/* Verify it's accepted on char member arrays.  */
  NONSTR char mns1[1];
  char NONSTR mns3[3];
  char mns5[5] NONSTR;

/* Verify it's accepted on char flexible array members.  */
  char mnsx[] NONSTR;
};

/* Verify it's rejected on non-array and non-pointer objects.  */
extern NONSTR char c1;         /* { dg-warning ".nonstring. attribute ignored on objects of type .char." } */

extern NONSTR int i1;         /* { dg-warning ".nonstring. attribute ignored on objects of type .int." } */

extern NONSTR int ia1[];      /* { dg-warning ".nonstring. attribute ignored on objects of type .int *\\\[\\\]." } */

extern NONSTR int* pi1;       /* { dg-warning ".nonstring. attribute ignored on objects of type .int *\\*." } */

extern NONSTR
void f (void);                /* { dg-warning ".nonstring. attribute does not apply to functions" } */

struct NONSTR
NonStrType { int i; };        /* { dg-warning ".nonstring. attribute does not apply to types" } */

typedef char NONSTR nschar_t; /* { dg-warning ".nonstring. attribute does not apply to types" } */

void func (NONSTR char *pns1, char NONSTR *pns2, char* NONSTR pns3)
{
  (void)pns1;
  (void)pns2;
  (void)pns3;
}
