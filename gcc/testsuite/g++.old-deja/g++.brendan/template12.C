// Build don't link: 
// GROUPS passed templates
inline void foo (const int &x) {}

template <class type>
inline void foo (const type &x) {x.eat_this_and_die();}

int main (int argc, char **argv) {foo (argc);}
