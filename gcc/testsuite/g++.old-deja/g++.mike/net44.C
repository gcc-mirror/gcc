// Make sure we don't dump core

enum request { q, w, e};		// ERROR - 

class request {				// ERROR - 
public:
  int a;
  request( int b) {
    a = b;
  };
};
