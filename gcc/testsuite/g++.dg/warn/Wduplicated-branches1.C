// PR c/64279
// { dg-do compile }
// { dg-options "-Wduplicated-branches" }

template <typename T>
void
f (signed char i, int *p)
{
  if (i) // { dg-warning "this condition has identical branches" "" { target short_eq_int } }
    *p = (signed short) i;
  else
    *p = (unsigned short) i;

  if (i) // { dg-warning "this condition has identical branches" }
    *p = (T) i;
  else
    *p = (unsigned short) i;
}

template void f<unsigned short>(signed char, int *); // { dg-message "required from here" }
template void f<signed short>(signed char, int *);
