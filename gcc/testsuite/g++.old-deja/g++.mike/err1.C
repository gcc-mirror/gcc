// { dg-do assemble  }

struct gorf {
  int stuff;
  void snarf();			// { dg-error "" } 
};

template <class T> void gorf::snarf() { return; }  // { dg-error "" } 
