// { dg-do assemble  }
class cb {
};

class cc {
public:
  cc()
    : cb() {		// { dg-error "" } 
  }
};
