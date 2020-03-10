// PR debug/93888
// { dg-do run }
// { dg-options "-g -fvar-tracking -fno-inline" }
// { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } }

struct K
{
  K () {}
  K (K const &rhs) { k[0] = 'C'; }
  char k[8] = {'B','B','B','B','B','B','B','B'};
};

__attribute__((always_inline)) inline bool
foo (const K karg)
{
  return karg.k[0] != 'C';	// { dg-final { gdb-test 16 "karg.k[0]" "'C'" } }
}				// { dg-final { gdb-test 16 "karg.k[1]" "'B'" } }

int
main ()
{
  K x;
  return foo (x);
}
