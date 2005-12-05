/* { dg-do compile } */
/* { dg-require-weak "" } */

typedef int vtype;

extern vtype wv1;
extern vtype Wv1a __attribute__((weakref ("wv1"))); /* { dg-error "weakref 'Wv1a' must have static linkage" } */
