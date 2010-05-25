/* __thread specifiers on empty declarations.  */
/* { dg-require-effective-target tls } */

__thread struct foo; /* { dg-warning "qualifiers can only be specified for objects and functions" } */
