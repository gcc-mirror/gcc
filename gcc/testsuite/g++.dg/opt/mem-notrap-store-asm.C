// Keep a normal load nontrapping.  A memory asm output is a store even when
// expansion must return a MEM.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fdump-rtl-expand" }

extern const int output;

int
load ()
{
  return output;
}

void
store ()
{
  asm volatile ("" : "=m" (const_cast<int &> (output)));
}

// { dg-final { scan-rtl-dump {\(mem(?:/[a-z])*/c(?:/[a-z])*:[^\n\r]*\[[0-9]+ output\+0} "expand" } }
// { dg-final { scan-rtl-dump {\(set[ \t\n]+\(mem(?:/[a-z])*:[^\n\r]*\[[0-9]+ output\+0} "expand" } }
// { dg-final { scan-rtl-dump-not {\(set[ \t\n]+\(mem(?:/[a-z])*/c(?:/[a-z])*:[^\n\r]*\[[0-9]+ output\+0} "expand" } }
