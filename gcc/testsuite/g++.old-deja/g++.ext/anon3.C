// { dg-do assemble  }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jun 2001 <nathan@codesourcery.com>

// Bug 2914. New types can be created in a static member
// initializer. These should not be injected into the member's class's
// scope.

class DoubleSupport
{
  public:
  static void toDouble();
  
  static const double s_NaN;
  static const double s_positiveInfinity;
  static const double s_negativeInfinity;
  static const double s_positiveZero;
  static const double s_negativeZero;
  static const unsigned long* s_NaNFirstDWORD;
  static const unsigned long* s_NaNSecondDWORD;
};

const double DoubleSupport::s_positiveInfinity =
(__extension__ ((union { unsigned char __c[8]; double __d; })
  { __c: { 0, 0, 0, 0, 0, 0, 0xf0, 0x7f } }).__d);

struct other 
{
};


void
DoubleSupport::toDouble()
{
}
