/* PR c/85931 - -Wsizeof-pointer-memaccess for strncpy with size of source
   { dg-do compile }
   { dg-require-effective-target alloca }
   { dg-options "-O2 -Wall -Wstringop-truncation -ftrack-macro-expansion=0" } */

typedef __SIZE_TYPE__ size_t;

extern char* strncpy (char*, const char*, size_t);

extern char a3[3], b3[3];
extern char a5[5], b5[5];
extern char ax[], bx[];

struct SA
{
  char a3[3], b3[3];
  char a5[5], b5[5];
  char ax[];
};

void sink (void*, ...);

#define T(d, s, n)   sink (strncpy (d, s, n))

void test_array (unsigned n)
{
  T (a3, b3, 3);
  /* For the following statemenmt, GCC 8.1 issues warning:

       argument to ‘sizeof’ in ‘strncpy’ call is the same expression
       as the source; did you mean to use the size of the destination?

     Since the size of both the source and destination the warning
     isn't helpful.  Verify that it isn't issued.  */
  T (a3, b3, sizeof b3);    /* { dg-bogus "\\\[-Wsizeof-pointer-memaccess" } */

  T (a3, ax, sizeof a3);    /* { dg-warning "\\\[-Wstringop-truncation" } */
  T (ax, a3, sizeof a3);    /* { dg-warning "argument to .sizeof. in .strncpy. call is the same expression as the source" } */

  char an[n], bn[n];
  sink (an, bn);

  T (an, bn, sizeof bn);    /* { dg-bogus "\\\[-Wsizeof-pointer-memaccess" } */
}

void test_member_array (struct SA *sa, unsigned n)
{
  T (sa->a3, sa->b3, 3);
  T (sa->a3, sa->b3, sizeof sa->b3);  /* { dg-bogus "\\\[-Wsizeof-pointer-memaccess" } */

  T (sa->a3, sa->ax, sizeof sa->a3);  /* { dg-warning "\\\[-Wstringop-truncation" } */
  T (sa->ax, sa->a3, sizeof sa->a3);  /* { dg-warning "argument to .sizeof. in .strncpy. call is the same expression as the source" } */

  struct VarLenStruct {
    char an[n], bn[n];
  } x;

  sink (&x);
  T (x.an, x.bn, sizeof x.bn);        /* { dg-bogus "\\\[-Wsizeof-pointer-memaccess" } */
}
