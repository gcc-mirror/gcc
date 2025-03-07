/* Test to exercise attribute "nonstring" syntax.
   { dg-do compile }
   { dg-options "-Wattributes" }  */

#define ATTR(list) __attribute__ (list)
#define NONSTR     ATTR ((nonstring))

/* Verify it's accepted on char[] arrays.  */
extern NONSTR char nsx_1[][3];
extern char NONSTR nsx_2[][3];
extern char nsx_3[][3] NONSTR;

extern NONSTR char ns1[1][4];
extern char NONSTR ns2[3][5];
extern char ns3[5][6] NONSTR;

extern NONSTR char ns4[1][2][3][4];
extern char NONSTR ns5[2][3][4][5][6];
extern char ns6[1][2][3][1][2][3][1][2][3] NONSTR;

/* Verify it's accepted on char[] pointers.  */
extern NONSTR char (*pns_1)[3];
extern char NONSTR (*pns_2)[4];
extern char (*NONSTR pns_3)[5];

extern NONSTR char (*pns_4)[1][2];
extern char NONSTR (*pns_5)[2][3][1][7][4];
extern char (*NONSTR pns_6)[1][1][1][2][1][1][1][2][1][1][1][2][1][1][7];

struct S
{
/* Verify it's accepted on char[] member pointers.  */
  NONSTR char (*mpns_1)[3];
  char NONSTR (*mpns_2)[4];
  char (*NONSTR mpns_3)[5];

/* Verify it's accepted on char[] member arrays.  */
  NONSTR char mns1[1][2];
  char NONSTR mns3[3][3];
  char mns5[5][4] NONSTR;

/* Verify it's accepted on char[] flexible array members.  */
  char mnsx[][5] NONSTR;
};

void func (NONSTR char (*pns1)[2], char NONSTR (*pns2)[3], char (* NONSTR pns3)[4])
{
  (void)pns1;
  (void)pns2;
  (void)pns3;
}
