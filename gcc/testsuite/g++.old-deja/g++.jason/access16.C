// Bug: g++ uses the same binfo for the a subobject of c and the a subobject
// of b, so basetype_paths get bashed improperly.
// Build don't link:

class a {
protected:
	virtual void foo() { }	// gets bogus error
};

class b : public virtual a {};

class c : public b {
public:
	void bar() { b::foo(); } // gets bogus error
};

int main() {
	c test;
	test.bar();
}
