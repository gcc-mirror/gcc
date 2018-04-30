/* PR c/79983 */
/* { dg-do compile } */
/* { dg-options "" } */

struct S;
struct S { int i; }; /* { dg-message "originally defined here" } */
struct S { int i, j; }; /* { dg-error "redefinition of 'struct S'" } */

enum E;
enum E { A, B, C }; /* { dg-message "originally defined here" } */
enum E { D, F }; /* { dg-error "nested redefinition of 'enum E'|redeclaration of 'enum E'" } */

union U;
union U { int i; }; /* { dg-message "originally defined here" } */
union U { int i; double d; }; /* { dg-error "redefinition of 'union U'" } */
