/* Test that we don't let stmt.c think that the enumeration's values are
   the entire set of possibilities.  Such an assumption is false for C,
   but true for other languages.  */

enum X { X1 = 1, X2, X3, X4 };
static volatile enum X test = 0;
static void y(int);

int main()
{
  switch (test)
    {
    case X1: y(1); break;
    case X2: y(2); break;
    case X3: y(3); break;
    case X4: y(4); break;
    }
  return 0;
}

static void y(int x) { abort (); }
