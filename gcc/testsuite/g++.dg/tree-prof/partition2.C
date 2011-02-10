// PR middle-end/45458
// { dg-require-effective-target freorder }
// { dg-options "-fnon-call-exceptions -freorder-blocks-and-partition" }
// { dg-skip-if "PR target/47683" { mips-sgi-irix* } }

int
main ()
{
  try
  {
    throw 6;
  }
  catch (...)
  {
  }
}
