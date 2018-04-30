// { dg-do assemble  }

struct gorf { // { dg-message "defined here" }
  int stuff;
  void snarf();			// { dg-message "" } 
};

template <class T> void gorf::snarf() { return; }  // { dg-error "no declaration" } 
