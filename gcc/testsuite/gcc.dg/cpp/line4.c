/* { dg-do compile } */

/* Test #line with and without macros for the line number.  */

extern void abort (void);

#define L 90

#line 44
enum { i = __LINE__ };

#line L
enum { j = __LINE__ };

#line 16  /* N.B. the _next_ line is line 16.  */

char array1[i        == 44 ? 1 : -1];
char array2[j        == 90 ? 1 : -1];
char array3[__LINE__ == 19 ? 1 : -1];
