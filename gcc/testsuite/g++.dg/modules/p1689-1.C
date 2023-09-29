// { dg-additional-options -E }
// { dg-additional-options "-MT p1689-1.ddi" }
// { dg-additional-options -MD }
// { dg-additional-options -fmodules-ts }
// { dg-additional-options -fdeps-format=p1689r5 }
// { dg-additional-options -fdeps-target=p1689-1.o }
// { dg-additional-options -fdeps-file=p1689-1.ddi }

// Export a module that uses modules, re-exports modules, and partitions.

export module foo;
export import foo:part1;
import foo:part2;

export import bar;

// { dg-final { run-check-p1689-valid p1689-1.ddi p1689-1.exp.ddi } }
