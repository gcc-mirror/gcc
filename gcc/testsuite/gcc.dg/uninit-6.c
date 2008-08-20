/* Spurious uninitialized variable warnings.
   This one inspired by java/class.c:build_utf8_ref.  */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

#include <stddef.h>

struct tree
{
    struct tree *car;
    struct tree *cdr;
    int type, data;
};

extern void *malloc(size_t);

#define INTEGER_T 1
#define PTR_T	  2

#define APPEND(TREE, LAST, TYPE, VALUE)				\
do {								\
     struct tree *tmp = malloc (sizeof (struct tree));		\
     tmp->car = 0; tmp->cdr = 0; tmp->type = TYPE;		\
     tmp->data = VALUE;						\
     if (TREE->car)						\
	 LAST->cdr = tmp;					\
     else							\
	 TREE->car = tmp;					\
     LAST = tmp;						\
} while(0)
 
struct tree *
make_something(int a, int b, int c)
{
    struct tree *rv;
    struct tree *field;

    rv = malloc (sizeof (struct tree));
    rv->car = 0;

    APPEND(rv, field, INTEGER_T, a);  /* { dg-bogus "field" "uninitialized variable warning" } */
    APPEND(rv, field, PTR_T, b);
    APPEND(rv, field, INTEGER_T, c);

    return rv;
}
