// Build don't link:

struct T {
  struct S __attribute__ ((packed)) { // ERROR - parse error
    int i;                            
  };
}; // ERROR - parse error
