// Build don't link: 
// GROUPS passed visibility
template <class T> 
class Feld {
public:
  Feld(const Feld&) {}
};

class Polynom : private Feld<double> {
  Polynom();
  friend Polynom f(const Polynom&);
};

Polynom f(const Polynom& p) { return p; }    
