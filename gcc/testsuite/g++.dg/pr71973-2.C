// { dg-do compile }
// { dg-options "-Wall -fdump-tree-eh" }

typedef __SIZE_TYPE__ size_t;
struct tm;

extern "C"
size_t strftime (char*, size_t, const char*, const struct tm*)
__attribute__ ((__nothrow__));

void foo () throw ()
{
  strftime (0,0,0,0); // { dg-warning "argument \(1|3|4\) null where non-null expected" }
  // { dg-warning "too many arguments for format" "" { target *-*-* } .-1 }
}

// { dg-final { scan-tree-dump-not "eh_dispatch" "eh" } }
// { dg-final { scan-tree-dump-not "resx" "eh" } }
