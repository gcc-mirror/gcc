/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
int v1;			/* { dg-message "previous declaration" } */
int v1;			/* { dg-warning "invalid in C\[+\]\[+\]" } */
int v2;			/* { dg-message "previous declaration" } */
int v2 = 1;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
extern int v3;
int v3;			/* { dg-message "previous declaration" } */
int v3 = 1;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
extern int v4;
int v4 = 1;
static int v5;		/* { dg-message "previous declaration" } */
static int v5;		/* { dg-warning "invalid in C\[+\]\[+\]" } */
static int v6;		/* { dg-message "previous declaration" } */
static int v6 = 1;	/* { dg-warning "invalid in C\[+\]\[+\]" } */
int v7;
extern int v7;
int v8 = 1;
extern int v8;
