// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs
class X {
public:
    enum e {
	New,// { dg-error "" }  conflicts with other.*
    }; // { dg-error "" } comma

    static int New(int);// { dg-error "" }  declaration.*
};

int main() {}
