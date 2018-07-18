/* { dg-options "-fdiagnostics-show-caret -Wpedantic" } */

/* Struct with extra semicolon.  */
struct s1 { int i;; }; /* { dg-warning "19: extra semicolon in struct or union specified" } */
/* { dg-begin-multiline-output "" }
 struct s1 { int i;; };
                   ^
                   -
   { dg-end-multiline-output "" } */

/* Union with extra semicolon.  */
union u1 { int i;; }; /* { dg-warning "18: extra semicolon in struct or union specified" } */
/* { dg-begin-multiline-output "" }
 union u1 { int i;; };
                  ^
                  -
   { dg-end-multiline-output "" } */
