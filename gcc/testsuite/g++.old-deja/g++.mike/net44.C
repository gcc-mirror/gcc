// { dg-do assemble  }
// Make sure we don't dump core

enum request { q, w, e};		// { dg-message "" } 

class request {				// { dg-error "" } 
public:
  int a;
  request( int b) {
    a = b;
  };
};
