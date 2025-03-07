/* Test to exercise attribute "nonstring".
   { dg-do compile }
   { dg-options "-O2 -Wattributes -Wstringop-truncation -ftrack-macro-expansion=0" }  */

#define ATTR(list) __attribute__ (list)
#define NONSTR     ATTR ((nonstring))
#define strncpy(d, s, n) (__builtin_strncpy ((d), (s), (n)), sink (d))

void sink (void*);

/* Global string with an known bound.  */
extern char gns3[][3] NONSTR;

/* Global non-string pointers.  */
extern NONSTR char (*pns_1)[1];
extern char (* NONSTR pns_2)[2];
extern char (*pns_3)[3] NONSTR;

struct MemArrays
{
  NONSTR char ma3[2][3];
  char NONSTR ma4[3][4];
  char ma5[4][5] NONSTR;
};


void test_array (const char *s, unsigned n)
{
  const char s7[] = "1234567";

  strncpy (gns3[2], "", 0);
  strncpy (gns3[2], "a", 1);
  strncpy (gns3[2], "a", 2);
  strncpy (gns3[2], "a", 3);
  strncpy (gns3[2], "ab", 3);
  strncpy (gns3[2], "abc", 3);
}


void test_pointer (const char *s, unsigned n)
{
  const char s7[] = "1234567";

  strncpy (*pns_1, "a", 1);
  strncpy (*pns_2, "ab", 2);
  strncpy (*pns_3, "abc", 3);
  strncpy (*pns_3, s7, 3);

  strncpy (*pns_1, s, 1);
  strncpy (*pns_2, s, 1);
  strncpy (*pns_3, s, 1);

  strncpy (*pns_1, s, n);
  strncpy (*pns_2, s, n);
  strncpy (*pns_3, s, n);
}


void test_member_array (struct MemArrays *p, const char *s, unsigned n)
{
  const char s7[] = "1234567";

  strncpy (p->ma3[1], "", 0);
  strncpy (p->ma3[1], "a", 1);
  strncpy (p->ma4[2], "ab", 2);
  strncpy (p->ma5[3], "abc", 3);

  strncpy (p->ma3[1], s, 1);
  strncpy (p->ma4[2], s, 1);
  strncpy (p->ma5[3], s, 1);

  strncpy (p->ma3[1], s7, n);
  strncpy (p->ma4[2], s7, n);
  strncpy (p->ma5[3], s7, n);
}
