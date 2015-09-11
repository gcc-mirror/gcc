// { dg-do assemble  }
// From: smidt@dd.chalmers.se (Peter Smidt)
// Date: 25 Jan 1994 23:41:33 -0500
// Bug: g++ forgets access decls after the definition.

class inh { // { dg-message "" } inaccessible
        int a;
protected:
        void myf(int);
};

class mel : private inh {
protected:
        int t;
	inh::myf;  // { dg-warning "deprecated" }
};

class top_t : protected mel {
public:
        void myf(int);
};

void inh::myf(int i) {
        a = i;
}

void top_t::myf(int i) {
        inh::myf(i);		// { dg-error "" } cannot convert to inh
	mel::myf(i);
}
