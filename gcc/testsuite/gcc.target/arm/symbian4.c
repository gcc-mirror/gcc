/* { dg-do compile { target arm*-*-symbianelf* } } */
/* Check that wchar_t is a 2-byte type.  */

extern int i[sizeof (L'a')];
int i[2];
