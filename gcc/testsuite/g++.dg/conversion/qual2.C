// PR c++/88128 - DR 330: Qual convs and pointers to arrays of pointers.

// Make sure we don't accept different bounds.

int *a[4];
const int *const(*ap1)[5] = &a; // { dg-error "cannot convert" }

int *(*b)[3];
const int *const (*bp1)[3] = &b; // { dg-error "cannot convert" }
const int *const (*bp2)[4] = &b; // { dg-error "cannot convert" }
int *(*bp3)[4] = &b; // { dg-error "cannot convert" }

int *c[2][3];
int const *const (*cp1)[4] = c; // { dg-error "cannot convert" }
