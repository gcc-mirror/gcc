// Build don't link:
// Special g++ Options: -Wall

enum tristate { no = -1, maybe, yes };

tristate
definite_tristate (int truth)
{
  return (truth) ? yes : no;
}
