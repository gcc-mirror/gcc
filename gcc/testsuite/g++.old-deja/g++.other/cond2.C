// { dg-do assemble  }
// Origin: Loring Holden <lsh@cs.brown.edu>

class Wpt {};

class RAYhit {
   protected:
      Wpt       _nearpt;
   public:
      Wpt       surf        () const { return true ? Wpt(): _nearpt; } 
};
