// A memory-only asm input is a load even though expansion must return a MEM.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fdump-rtl-expand" }

extern const int input;

void
load ()
{
  asm volatile ("" : : "m" (input));
}

// { dg-final { scan-rtl-dump {\(mem(?:/[a-z])*/c(?:/[a-z])*:[^\n\r]*\[[0-9]+ input\+0} "expand" } }
