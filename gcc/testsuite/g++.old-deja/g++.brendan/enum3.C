// Build don't link: 
// GROUPS passed enums
enum foo
{
  x = 0
};

enum bar
{
  // this used to say `x' wasn't a constant, because build_enumerator
  // was getting the value of x wrapped around a NOP_EXPR.  It now
  // strips them off before working on it, so we shouldn't get any
  // errors  for this.
  y = (x + 0x0000)
};
