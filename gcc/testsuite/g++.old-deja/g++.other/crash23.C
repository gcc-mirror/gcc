// Build don't link:
// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

class T;
inline void operator<(T&, T&) { } // ERROR -  previous definition
inline void operator<(T&, T&) { } // ERROR - duplicate definition

