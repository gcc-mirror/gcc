/* { dg-options "-fshort-enums" } */
/* Check that "-fshort-enums" packs enumeration tyes into a minimal
   number of bytes..  */

enum e { e_1 };

extern int i[sizeof (enum e)];
int i[1];
