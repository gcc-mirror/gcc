// DR 1512
// PR c++/87699
// { dg-do compile { target c++11 } }

/* Relational comparisons between null pointer constants and pointers are now
   ill-formed.  */

void
f (char *p)
{
  if (p > 0) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p >= 0) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p < 0) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p <= 0) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p > nullptr) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p >= nullptr) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p < nullptr) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (p <= nullptr) { } // { dg-error "ordered comparison of pointer with integer zero" }
}

void
f2 (char *p)
{
  if (0 > p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (0 >= p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (0 < p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (0 <= p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (nullptr > p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (nullptr >= p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (nullptr < p) { } // { dg-error "ordered comparison of pointer with integer zero" }
  if (nullptr <= p) { } // { dg-error "ordered comparison of pointer with integer zero" }
}
