// { dg-do assemble  }
// { dg-options "-Wall -Wno-register" }

enum tristate { no = -1, maybe, yes };

tristate
tristate_satisfies (register tristate const t1, register tristate const t2)
{
  switch (t1)
    {
    case no:
      return (tristate) -t2;
    case maybe:
      return yes;
    case yes:
      return t2;
    }
  return maybe;
}
