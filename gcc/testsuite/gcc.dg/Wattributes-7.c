/* PR c/81566 - invalid attribute aligned accepted on functions
   { dg-do compile }
   { dg-options "-Wall -Wattributes -ftrack-macro-expansion=0" } */

#define ATTR(list) __attribute__ (list)
#define ALIGN(n)   ATTR ((aligned (n)))

/* It's okay to increase the alignment of a function.  */

void ALIGN (16) ALIGN (32)
falign32_1 (void);

void ALIGN (16) falign32_2 (void);
void ALIGN (32) falign32_2 (void);

void falign32_2 (void) { }

void ALIGN (32) falign32_2 (void);

/* It's not okay to decrease it.  */

void ALIGN (32) ALIGN (16)
falign64_3 (void);            /* { dg-warning "ignoring attribute .aligned \\(16\\). because it conflicts with attribute .aligned \\(32\\)." } */

void ALIGN (32)
falign64_3 (void);

void falign64_3 (void);

void falign64_3 (void) { }


void ALIGN (32)
falign64_4 (void);            /* { dg-message "previous declaration here" } */

void ALIGN (16)
falign64_4 (void);            /* { dg-warning "ignoring attribute .aligned \\(16\\). because it conflicts with attribute .aligned \\(32\\)." } */

void ALIGN (32)
falign64_4 (void);            /* { dg-message "previous declaration here" } */

void ALIGN (16)
falign64_4 (void);            /* { dg-warning "ignoring attribute .aligned \\(16\\). because it conflicts with attribute .aligned \\(32\\)." } */

void ALIGN (64)
falign64_4 (void);

void ALIGN (32)
falign64_4 (void);            /* { dg-warning "ignoring attribute .aligned \\(32\\). because it conflicts with attribute .aligned \\(64\\)." } */

void falign64_4 (void);

void ALIGN (64)
falign64_4 (void) { }

void falign64_4 (void);

void ALIGN (64)
falign64_4 (void);


void ATTR ((aligned (16), aligned (32)))
falign64_5 (void);

void ATTR ((aligned (32), aligned (64)))
falign64_5 (void);

void ATTR ((aligned (16), aligned (32), aligned (64)))
falign64_5 (void);            /* { dg-warning "ignoring attribute .aligned \\(16\\). because it conflicts with attribute .aligned \\(64\\)." } */
                              /* { dg-warning "ignoring attribute .aligned \\(32\\). because it conflicts with attribute .aligned \\(64\\)." "" { target *-*-* } .-1 } */


void ATTR ((aligned (16), aligned (32), aligned (16)))
falign64_6 (void);            /* { dg-warning "ignoring attribute .aligned \\(16\\). because it conflicts with attribute .aligned \\(32\\)." } */
