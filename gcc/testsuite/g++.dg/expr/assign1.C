// { dg-do run }

// Contributed by Nathan Sidwell 15 Dec 2003 <nathan@codesourcery.com>
// Origin: Tasso Karkanis <Tasso.Karkanis@rogers.com>
// PR c++/13387. Clobbered tail padding of base

inline void *operator new (__SIZE_TYPE__, void *ptr) 
{
  return ptr;
}

struct Base { 
    Base() : i(0), c(0) {} 
    int i; 
    char c; 
}; 
 
struct Sub : Base { 
    Sub () : d(0) {} 
    char d; 
}; 
 
int main() { 
  Sub sub;
  char base_alias[sizeof (Base)];
  Base *base;
  
  for (unsigned ix = sizeof base_alias; ix--;)
    base_alias[ix] = 0x55;
  base = new (&base_alias) Base ();
  
  static_cast <Base &> (sub) = *base;
  return sub.d; 
} 
