// PRs 16387 and 16389
// We were treating alignof (sa.a) as alignof (typeof (sa.a)), which is
// wrong for some fields.

// { dg-do run }
// { dg-xfail-run-if "AIX/Darwin ABI increases struct alignment for first member double" { powerpc-ibm-aix* || { ilp32 && powerpc-*-darwin* } } }

extern "C" void abort();

struct A
{
  double a; 
} sa;

struct B
{
  char c;
  double b;
} sb;

int main()
{
  if (__alignof (sa) != __alignof (sa.a)
      || __alignof (sb) != __alignof (sb.b))
    abort();
}
