/* PR middle-end/94647 - bogus -Warray-bounds on strncpy into a larger
   member array from a smaller array
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern char* strncpy (char*, const char*, size_t);


char a4[4], a8[8];

void nowarn_nonmeber (void)
{
  /* The following would deserve a warning if A4 were known not to be
     nul-terminated (or declared with attribute nonstring).  */
  strncpy (a8, a4, sizeof a8);
}
struct S
{
  char a4[4], a8[8];
};

void nowarn_member (struct S *p, struct S *q)
{
  /* The following would deserve a warning if A4 were known either
     not to be nul-terminated (e.g., declared nonstring) or to be
     uninitialized.  */
  strncpy (p->a8, p->a4, sizeof p->a8);   // { dg-bogus "\\\[-Warray-bounds" }
}
