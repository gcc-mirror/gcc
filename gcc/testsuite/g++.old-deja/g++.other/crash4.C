// { dg-do assemble  }

struct T {
  struct S __attribute__ ((packed)) { // { dg-error "" } parse error
    int i;                            
  };
};
