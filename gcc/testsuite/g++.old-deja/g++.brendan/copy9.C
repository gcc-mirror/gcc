// GROUPS passed copy-ctors
#include <iostream>

// token types: from state parser
const int T_EOF = 257;
const int T_ERROR = 258;
const int T_Float = 259;
const int T_Int = 260;
const int T_ID = 261;
const int T_STRING = 262;

class Complex;
class State;

// token, from state parser.
class ParseToken {
public:
	int tok;
	union {
		char cval;
		const char *sval;
		int intval;
		double  doubleval;
		Complex* Complexval;
		const State*  s;
	}; 
	ParseToken () { tok = 0; intval = 0;}
};

int
main () {
	ParseToken a;
	a.tok = T_Float;
	a.doubleval = 23.2;
	ParseToken b(a);

	if (b.doubleval == 23.2)
	  std::cout << "PASS\n";
	else
	  {
	    std::cout << "FAIL\n";
	    return 1;
	  }
}

