// { dg-do assemble  }
// GROUPS passed initialization
  class Thing{
  private:
	  int x,y;
  public:
	  Thing (int v, int q) { x = v; q = y; }
	  void doit(int);
  };

  Thing t(18,19);
