// { dg-do assemble  }
// Origin: Loring Holden <lsh@cs.brown.edu>

class Wtransf {};

const Wtransf Identity2 = Wtransf ();

class HELPER {
   public:
      int  current() const  { return 0; }
};

void
problem_function()
{
   HELPER tm;
   Wtransf delta  = (tm.current()) ? Identity2 : Wtransf();
}
