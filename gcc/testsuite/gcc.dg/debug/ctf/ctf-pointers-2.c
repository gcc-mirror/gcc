/* CTF generation for pointer types.

   In this testcase, de-duplication of pointer types is exercised.  The
   compostition of structs in this testcase is such that when adding CTF for
   pointer type (link), the pointed-to-type type already adds the pointer to
   struct link.
   
   In this testcase, one CTF pointer type record is expected.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*ctt_info" 1 } } */

struct link;

typedef struct items {
    struct link * link; 
    int str;
} itemslist;

itemslist il;

struct link { struct link * next; };

