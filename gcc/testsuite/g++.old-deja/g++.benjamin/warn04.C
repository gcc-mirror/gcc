// 980903 bkoz
// make sure this option works
// Build don't link: 
// Special g++ Options: -Wno-non-template-friend


template<class T> class task {
  friend void next_time(); //shouldn't give a warning
};
