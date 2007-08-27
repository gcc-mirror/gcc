/* { dg-do compile { target arm*-*-symbianelf* } } */
/* Check that enumeration types are 4-byte types.  */

enum e { e_1 };

extern int i[sizeof (enum e)];
int i[4];
