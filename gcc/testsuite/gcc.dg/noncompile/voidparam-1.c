/* Test for bad uses of 'void' in parameter lists.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

typedef const void cv;
typedef volatile void vv;

void foo0 (const void); /* { dg-error "parameter" "const void decl" } */
void foo0a (cv); /* { dg-error "parameter" "const void decl" } */
void foo1 (volatile void); /* { dg-error "parameter" "volatile void decl" } */
void foo1a (vv); /* { dg-error "parameter" "volatile void decl" } */
void foo2 (register void); /* { dg-error "parameter" "register void decl" } */

void bar0 (const void) { } /* { dg-error "parameter" "const void defn" } */
void bar0a (cv) { } /* { dg-error "parameter" "const void defn" } */
void bar1 (volatile void) { } /* { dg-error "parameter" "volatile void defn" } */
void bar1a (vv) { } /* { dg-error "parameter" "volatile void defn" } */
void bar2 (register void) { } /* { dg-error "parameter" "register void defn" } */
