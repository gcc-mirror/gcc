// prms-id: 10247

class a {
public:
  int operator++(int) { return operator()++ ; }		// ERROR - 
};
