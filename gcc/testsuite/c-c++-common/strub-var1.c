/* { dg-do compile } */
/* { dg-require-effective-target strub } */

int __attribute__ ((strub)) x;
float __attribute__ ((strub)) f;
double __attribute__ ((strub)) d;

/* The attribute applies to the type of the declaration, i.e., to the pointer
   variable p, not to the pointed-to integer.  */
int __attribute__ ((strub)) *
p = &x; /* { dg-message "incompatible|invalid conversion" } */

typedef int __attribute__ ((strub)) strub_int;
strub_int *q = &x; /* Now this is compatible.  */

int __attribute__ ((strub))
a[2]; /* { dg-warning "does not apply to elements" } */

int __attribute__ ((vector_size (4 * sizeof (int))))
    __attribute__ ((strub))
v; /* { dg-warning "does not apply to elements" } */

struct s {
  int i, j;
} __attribute__ ((strub)) w; /* { dg-warning "does not apply to fields" } */
