// { dg-do assemble  }

// Origin: <mikes@nilenet.com>

// Bug: Specialization of implicitly created function should be
// rejected.

template<class T> class blah{};
blah<char>::blah(){}		// { dg-error "" } invalid specialization
int main(){}
