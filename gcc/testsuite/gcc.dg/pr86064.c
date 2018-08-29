/* { dg-do compile } */
/* { dg-skip-if "split DWARF unsupported" { *-*-darwin* } } */
/* { dg-options "-g -O2 -fno-var-tracking-assignments -gsplit-dwarf" } */

/* This used to fail with location views (implicitly) enabled, because
   var-tracking (not at assignments) creates a range for d starting at
   the load after the first call, and we did not split the range,
   despite its crossing between hot and cold partitions, because it's
   a single range, that we normally extend to the entire function.
   However, because the range starts at a (presumed) nonzero view, we
   end up outputting a loclist instead of a single location entry.
   But then, -gsplit-dwarf selects (startx,length) loclist entries,
   and the length ends up computing the difference between symbols in
   different subsections.  */

int a;
__attribute__((__cold__)) void b();

void e(int *);
int f();

void c() {
  int d;
  e(&d);
  a = d;
  if (f())
    b();
}
