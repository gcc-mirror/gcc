// { dg-do assemble  }
// 981203 bkoz
// g++/13523

template<typename T> class latin_america;

class peru
{
  friend class latin_america<int>;  // Particular template class friend works
  template<class T> friend class latin_america; // This does not work.
};

