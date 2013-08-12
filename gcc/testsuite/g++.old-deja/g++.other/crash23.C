// { dg-do assemble  }
// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

class T;
inline void operator<(T&, T&) { } // { dg-message "" }  previous definition
inline void operator<(T&, T&) { } // { dg-error "" } duplicate definition

