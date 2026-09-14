// Keep watermark loads nontrapping when a strub context calls another.

// { dg-do compile }
// { dg-options "-O2 -fstrub=strict -fnon-call-exceptions" }
// { dg-additional-options "-fdelete-null-pointer-checks -fdump-rtl-expand" }
// { dg-require-effective-target strub }
// { dg-skip-if "" keeps_null_pointer_checks }

extern int callee (int)
  __attribute__ ((__strub__ ("at-calls"), __nothrow__));
volatile int sink;

__attribute__ ((__strub__ ("at-calls"), __noinline__))
int
caller (int value)
{
  int result = callee (value);
  sink = result;
  return result + 1;
}

// The incoming watermark is loaded once by update and once by leave.
// { dg-final { scan-rtl-dump-times {\(set[ \t\n]+\(reg(?:/[a-z])*:[^\n\r]*\)[ \t\n]+\(mem(?:/[a-z])*/c(?:/[a-z])*:[^\n\r]*[*][.]strub[.]watermark_ptr} 2 "expand" } }
