// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions -fno-delete-dead-exceptions" }

// PR target/116927
// aarch64's Early ra was removing possiblely trapping
// floating point insn

void
foo (float f)
{
  try {
    f ++;
  }catch(...)
  {}
}
