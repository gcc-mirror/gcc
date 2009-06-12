/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

int and;			/* { dg-warning "operator" } */
int and_eq;			/* { dg-warning "operator" } */
int bitand;			/* { dg-warning "operator" } */
int bitor;			/* { dg-warning "operator" } */
int compl;			/* { dg-warning "operator" } */
int not;			/* { dg-warning "operator" } */
int not_eq;			/* { dg-warning "operator" } */
int or;				/* { dg-warning "operator" } */
int or_eq;			/* { dg-warning "operator" } */
int xor;			/* { dg-warning "operator" } */
int xor_eq;			/* { dg-warning "operator" } */

#define M1 and			/* { dg-warning "operator" } */
#define M2 and_eq		/* { dg-warning "operator" } */
#define M3 bitand		/* { dg-warning "operator" } */
#define M4 bitor		/* { dg-warning "operator" } */
#define M5 compl		/* { dg-warning "operator" } */
#define M6 not			/* { dg-warning "operator" } */
#define M7 not_eq		/* { dg-warning "operator" } */
#define M8 or			/* { dg-warning "operator" } */
#define M9 or_eq		/* { dg-warning "operator" } */
#define M10 xor			/* { dg-warning "operator" } */
#define M11 xor_eq		/* { dg-warning "operator" } */
