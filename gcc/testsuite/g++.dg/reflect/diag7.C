// PR c++/127802
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<typename T> int fn (T) { return 0; }

int i = [:^^fn:](0); // { dg-error "expected .template. keyword before dependent template name" }

template<auto R>
int
f ()
{
  return [:R:](0);  // { dg-error "expected .template. keyword before dependent template name" }
}

template<auto>
void
uninst ()
{
  [:^^fn:]<int>;  // { dg-error "expected .template. keyword before dependent template name" }
}

void
g ()
{
  f<^^fn> ();
}
