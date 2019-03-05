// { dg-do assemble  }
// prms-id: 7635

class DaycountBasis {
  mutable const int * p;
  mutable int * const q;	// { dg-error "3:.const. .q. cannot be declared .mutable." } 
};
