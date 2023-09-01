// { dg-additional-options -E }
// { dg-additional-options "-MT p1689-3.ddi" }
// { dg-additional-options -MD }
// { dg-additional-options -fmodules-ts }
// { dg-additional-options -fdeps-format=p1689r5 }
// { dg-additional-options -fdeps-target=p1689-3.o }
// { dg-additional-options -fdeps-file=p1689-3.ddi }

// Provide a module partition.

module foo:part2;

// { dg-final { run-check-p1689-valid p1689-3.ddi p1689-3.exp.ddi } }
