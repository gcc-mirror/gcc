/* PR c/105131 */
/* { dg-do compile } */
/* { dg-options "-Wall -fno-short-enums" } */

enum E { E1 = 0, E2, E3 };

unsigned int foo(void); /* { dg-message "previous declaration" } */
enum E foo(void) { return E2; } /* { dg-warning "conflicting types" } */

void bar(unsigned int); /* { dg-message "previous declaration" } */
void bar(enum E); /* { dg-warning "conflicting types" } */

extern enum E arr[10]; /* { dg-message "previous declaration" } */
extern unsigned int arr[10]; /* { dg-warning "conflicting types" } */

extern unsigned int i; /* { dg-message "previous declaration" } */
extern enum E i; /* { dg-warning "conflicting types" } */

extern unsigned int *p; /* { dg-message "previous declaration" } */
extern enum E *p; /* { dg-warning "conflicting types" } */

enum E foo2(void) { return E2; } /* { dg-message "previous definition" } */
unsigned int foo2(void); /* { dg-warning "conflicting types" } */

void bar2(enum E); /* { dg-message "previous declaration" } */
void bar2(unsigned int); /* { dg-warning "conflicting types" } */

extern unsigned int arr2[10]; /* { dg-message "previous declaration" } */
extern enum E arr2[10]; /* { dg-warning "conflicting types" } */

extern enum E i2; /* { dg-message "previous declaration" } */
extern unsigned int i2; /* { dg-warning "conflicting types" } */

extern enum E *p2; /* { dg-message "previous declaration" } */
extern unsigned int *p2; /* { dg-warning "conflicting types" } */

enum F { F1 = 1u, F2, F3 } __attribute__ ((__packed__));

enum F fn1(void); /* { dg-message "previous declaration" } */
unsigned char fn1(void); /* { dg-warning "conflicting types" } */

unsigned char fn2(void); /* { dg-message "previous declaration" } */
enum F fn2(void); /* { dg-warning "conflicting types" } */
