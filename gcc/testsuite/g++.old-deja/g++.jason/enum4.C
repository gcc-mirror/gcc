// { dg-do assemble  }
// { dg-options "-Wall" }

enum tristate { no = -1, maybe, yes };

tristate
definite_tristate (int truth)
{
  return (truth) ? yes : no;
}
