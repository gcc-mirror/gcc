// Build don't link:

struct gorf {
  int stuff;
  void snarf();			// ERROR - 
};

template <class T> void gorf::snarf() { return; }  // ERROR - 
