extern int r;
void *p;

#include "ctor2.h"

VBase::VBase ()
{
  p = this;
}

VBase::~VBase ()
{
  if (p != this) r = 1;
}

Stream::Stream () {}
DerivedStream::DerivedStream ()
{
  throw 1;
}
