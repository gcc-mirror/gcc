/* { dg-do compile } */
/* { dg-options "-fstrict-aliasing" } */

void* operator new(__SIZE_TYPE__, void* __p) throw()
{
  return __p;
}

class PatternDriverTop;

typedef const PatternDriverTop* _Tp;

void construct(_Tp* __p, const _Tp& __val)
{
  ::new((void *)__p) _Tp(__val);
}

