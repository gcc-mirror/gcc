// { dg-do assemble  }
// Bug: g++ uses the same binfo for the a subobject of c and the a subobject
// of b, so basetype_paths get bashed improperly.

class a {
protected:
	virtual void foo() { }	// { dg-bogus "" } 
};

class b : public virtual a {};

class c : public b {
public:
	void bar() { b::foo(); } // { dg-bogus "" } 
};

int main() {
	c test;
	test.bar();
}
