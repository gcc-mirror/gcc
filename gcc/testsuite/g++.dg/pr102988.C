/* { dg-do compile } */
/* { dg-options "-O -fharden-conditional-branches -fchecking=1" } */

/* DECL_BY_REFERENCE RESULT_DECL is read-only, we can't create a copy
   of its (address) default def and set it.  */

void ll();
struct k {
  ~k();
};
k ice(k *a)
{
  k v;
  if (&v!= a)
    ll();
  return v;
}
