// { dg-do assemble  }
// Origin: Miniussi <miniussi@ilog.fr>

template <class O>
struct Str {
  Str(int& val= (*new int())); 
};

template<class O>
Str<O>::Str(int& val) {}

