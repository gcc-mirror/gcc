// PR rtl-optimization/68392
// { dg-do compile }
// { dg-options "-O -fno-checking" }

void tool_cleanup(bool from_signal)
{
  tool_cleanup(from_signal);
}
