/* PR c/105131 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat -fno-short-enums" } */

enum E { E1 = -1, E2 = 0, E3 = 1 };

int foo(void); /* { dg-message "previous declaration" } */
enum E foo(void) { return E2; } /* { dg-warning "conflicting types" } */

void bar(int); /* { dg-message "previous declaration" } */
void bar(enum E); /* { dg-warning "conflicting types" } */

extern int arr[10]; /* { dg-message "previous declaration" } */
extern enum E arr[10]; /* { dg-warning "conflicting types" } */

extern int i; /* { dg-message "previous declaration" } */
extern enum E i; /* { dg-warning "conflicting types" } */

extern int *p; /* { dg-message "previous declaration" } */
extern enum E *p; /* { dg-warning "conflicting types" } */

enum E foo2(void) { return E2; } /* { dg-message "previous definition" } */
int foo2(void); /* { dg-warning "conflicting types" } */

void bar2(enum E); /* { dg-message "previous declaration" } */
void bar2(int); /* { dg-warning "conflicting types" } */

extern enum E arr2[10]; /* { dg-message "previous declaration" } */
extern int arr2[10]; /* { dg-warning "conflicting types" } */

extern enum E i2; /* { dg-message "previous declaration" } */
extern int i2; /* { dg-warning "conflicting types" } */

extern enum E *p2; /* { dg-message "previous declaration" } */
extern int *p2; /* { dg-warning "conflicting types" } */

enum F { F1 = -1, F2, F3 } __attribute__ ((__packed__));

enum F fn1(void); /* { dg-message "previous declaration" } */
signed char fn1(void); /* { dg-warning "conflicting types" } */

signed char fn2(void); /* { dg-message "previous declaration" } */
enum F fn2(void); /* { dg-warning "conflicting types" } */
