// PR c++/36191
// { dg-do compile }
// { dg-options "-fnon-call-exceptions" }
// { dg-skip-if "Frame pointer required for unwind tables" { m68k*-*-* fido*-*-* } "-fomit-frame-pointer" "" }

__complex__ double
foo (__complex__ double x, double y)
{
  try
    {
      return x / y;
    }
  catch (char *s)
    {
      return x;
    }
}
