// { dg-do assemble  }
// { dg-options "-Wno-non-template-friend" }
// 980903 bkoz
// make sure this option works


template<class T> class task {
  friend void next_time(); //shouldn't give a warning
};
