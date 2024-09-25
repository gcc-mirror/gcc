// { dg-additional-options -E }
// { dg-additional-options "-MT p1689-2.ddi" }
// { dg-additional-options -MD }
// { dg-additional-options -fmodules-ts }
// { dg-additional-options -fdeps-format=p1689r5 }
// { dg-additional-options -fdeps-target=p1689-2.o }
// { dg-additional-options -fdeps-file=p1689-2.ddi }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

// Export a module partition that uses modules.

export module foo:part1;

#include <iostream>

// { dg-final { run-check-p1689-valid p1689-2.ddi p1689-2.exp.ddi } }
