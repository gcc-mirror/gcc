// Error: Internal compiler error on 1998/05/28 snapshot.
// Build don't link:

class foo {
        typedef int sometype;
};

struct die : public foo::sometype { // ERROR - invalid base type
};
