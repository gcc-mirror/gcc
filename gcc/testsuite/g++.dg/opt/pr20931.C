// PR middle-end
// This testcase ICEd because fold checking saw a type change which
// is allowed as TYPE_CONTAINS_PLACEHOLDER_INTERNAL could change.
// { dg-do compile }
// { dg-options "-O2" }
  
int
__finite (double __x) throw ()
{
  return (__extension__
   (((((union { double __d; int __i[2]; }) {__d: __x}).__i[1]
      | 0x800fffffu) + 1) >> 31));
}
