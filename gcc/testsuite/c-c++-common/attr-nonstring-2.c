/* Test to exercise attribute "nonstring".
   { dg-do compile }
   { dg-options "-O2 -Wattributes -Wstringop-truncation -ftrack-macro-expansion=0" }  */

#define ATTR(list) __attribute__ (list)
#define NONSTR     ATTR ((nonstring))
#define strncpy(d, s, n) (__builtin_strncpy ((d), (s), (n)), sink (d))

void sink (void*);

/* Global string with an unknown bound.  */
extern char gsx[];

/* Global string with an known bound.  */
extern char gs3[3];

/* Global non-strings with an unknown bound.  */
extern NONSTR char gax_1[];
extern char NONSTR gax_2[];
extern char gax_3[] NONSTR;

/* Global non-strings with a known bound.  */
NONSTR char gns3[3];
char NONSTR gns4[4];
char gns5[5] NONSTR;

/* Global string pointer.  */
extern char *ps_1;

/* Global non-string pointers.  */
extern NONSTR char *pns_1;
extern char* NONSTR pns_2;
extern char *pns_3 NONSTR;

struct MemArrays
{
  NONSTR char ma3[3];
  char NONSTR ma4[4];
  char ma5[5] NONSTR;
  char max[] NONSTR;
};


void test_array (const char *s, unsigned n)
{
  const char s7[] = "1234567";

  strncpy (gs3, "", 0);           /* { dg-warning "destination unchanged after copying no bytes" } */
  strncpy (gs3, "a", 1);          /* { dg-warning "output truncated before terminating nul copying 1 byte from a string of the same length" } */
  strncpy (gs3, "a", 2);
  strncpy (gs3, "a", 3);
  strncpy (gs3, "ab", 3);
  strncpy (gs3, "abc", 3);        /* { dg-warning "output truncated before terminating nul copying 3 bytes from a string of the same length" } */

  /* It might perhaps be helpful to diagnose certain truncation even
     for non-strings.  Then again, since the destination has been
     explicitly annotated as non-string, it might be viewed as a false
     positive.  A valid use case seen in Glibc goes something like this:

     #if FOO
     # define S "1234"
     #else
     # define S "12345678"
     #endif

     strncpy (d, S, 8);
  */
  strncpy (gax_3, s7, 3);

  strncpy (gax_1, "a", 1);
  strncpy (gax_2, "ab", 2);
  strncpy (gax_3, "abc", 3);
  strncpy (gax_3, s7, 3);

  strncpy (gax_1, s, 1);
  strncpy (gax_2, s, 1);
  strncpy (gax_3, s, 1);

  strncpy (gax_1, s, n);
  strncpy (gax_2, s, n);
  strncpy (gax_3, s, n);
}


void test_pointer (const char *s, unsigned n)
{
  const char s7[] = "1234567";

  strncpy (pns_1, "a", 1);
  strncpy (pns_2, "ab", 2);
  strncpy (pns_3, "abc", 3);
  strncpy (pns_3, s7, 3);

  strncpy (pns_1, s, 1);
  strncpy (pns_2, s, 1);
  strncpy (pns_3, s, 1);

  strncpy (pns_1, s, n);
  strncpy (pns_2, s, n);
  strncpy (pns_3, s, n);
}


void test_member_array (struct MemArrays *p, const char *s, unsigned n)
{
  const char s7[] = "1234567";

  strncpy (p->ma3, "", 0);
  strncpy (p->ma3, "a", 1);
  strncpy (p->ma4, "ab", 2);
  strncpy (p->ma5, "abc", 3);
  strncpy (p->max, "abcd", 4);
  strncpy (p->max, s7, 5);

  strncpy (p->ma3, s, 1);
  strncpy (p->ma4, s, 1);
  strncpy (p->ma5, s, 1);
  strncpy (p->max, s, 1);

  strncpy (p->ma3, s7, n);
  strncpy (p->ma4, s7, n);
  strncpy (p->ma5, s7, n);
  strncpy (p->max, s7, n);
}
