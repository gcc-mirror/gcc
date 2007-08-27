/* { dg-do compile { target arm*-*-symbianelf* } } */
/* { dg-options "-fno-short-wchar" } */
/* Check that wchar_t is a 4-byte type when -fno-short-wchar is
   used.  */

extern int i[sizeof (L'a')];
int i[4];

