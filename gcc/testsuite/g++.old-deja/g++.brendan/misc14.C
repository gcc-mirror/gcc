// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs
class X {
public:
    enum e {
	New // { dg-error "conflicts with previous" }
	,   // { dg-error "comma at end" }
    };

    static int New(int); // { dg-error "declaration of" }
};

int main() {}
