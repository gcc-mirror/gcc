// Build don't link:

struct gorf {
  int stuff;
  void snarf();
};

template <class T> void gorf::snarf() { return; }  // ERROR - 
