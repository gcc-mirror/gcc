/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

typedef const int CI;
const const int c1;		/* { dg-error "duplicate" } */
const CI c2;			/* { dg-error "duplicate" } */
const CI *c3;			/* { dg-error "duplicate" } */

typedef volatile int VI;
volatile volatile int v1;	/* { dg-error "duplicate" } */
volatile VI v2;			/* { dg-error "duplicate" } */
volatile VI *v3;		/* { dg-error "duplicate" } */
