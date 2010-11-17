/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

typedef enum a { XYZ } a; /* { dg-message "previous declaration" } */
enum a a;	/* { dg-error "redeclared" } */
struct b { enum a a : 8; };
