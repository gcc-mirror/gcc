// prms-id: 7635

class DaycountBasis {
  mutable const int * p;
  mutable int * const q;	// ERROR - 
};
