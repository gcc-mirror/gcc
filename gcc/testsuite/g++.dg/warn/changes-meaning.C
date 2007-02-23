/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

template <class _Tp> class auto_ptr {};  /* { dg-warning "changes meaning" } */
template <class _Tp>
class counted_ptr
{
public:
  auto_ptr<_Tp> auto_ptr(); /* { dg-warning "" } */
};

