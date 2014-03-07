// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs
class X {
public:
    enum e {
	New // { dg-message "previous" }
	,   // { dg-error "comma at end" "" { target { ! c++11 } } }
    };

    static int New(int); // { dg-error "conflicts with a previous" }
};

int main() {}
