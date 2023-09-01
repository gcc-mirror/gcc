// { dg-additional-options -E }
// { dg-additional-options "-MT p1689-target-default.ddi" }
// { dg-additional-options -MD }
// { dg-additional-options -fmodules-ts }
// { dg-additional-options -fdeps-format=p1689r5 }
// { dg-additional-options -fdeps-file=p1689-target-default.ddi }

// Scan without `-fdeps-target=`

export module foo;
export import foo:part1;
import foo:part2;

export import bar;

// { dg-final { run-check-p1689-valid p1689-target-default.ddi p1689-target-default.exp.ddi } }
