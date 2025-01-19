/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

// There should be no warning about variably-modified types

static int a[0];
static int b[sizeof a];

void foo(int (*x)[*]);

static int c[0];
static int d[sizeof c];


