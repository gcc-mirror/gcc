// Build don't link: 
// Special g++ Options: -w
// GROUPS passed visibility
template <class T> 
class Feld {
public:
  Feld(const Feld&) {}
};

class Polynom : private Feld<double> {
friend Polynom f(const Polynom&);
};

Polynom f(const Polynom& p) { return p; }    
