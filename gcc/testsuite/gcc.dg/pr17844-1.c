/* -fshort-enums should affect only the type with which an enum is
    compatible, not the type of the enumeration constants which should
    still be int.  Bug 17844.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-fshort-enums" } */

enum e { A, B };

enum e ev;
unsigned char uv;
enum e *ep = &uv;
unsigned char *up = &ev;

int i;
__typeof__(A) te;
int *ip = &te;
__typeof__(B) *tep = &i;

int x[((sizeof(A) == sizeof(int)) ? 1 : -1)];
