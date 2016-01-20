/* { dg-do compile } */
/* { dg-options "-fsyntax-only" } */

static struct foo x;  /* { dg-error "storage size of 'x' isn't known" } */
static union bar y;  /* { dg-error "storage size of 'y' isn't known" } */

typedef struct P p;
static p p_obj;  /* { dg-error "storage size of 'p_obj' isn't known" } */

static enum e e_var; /* { dg-error "storage size of 'e_var' isn't known" } */

extern struct undefined_object object;
