
// Error: Internal compiler error on 1998/05/28 snapshot.

        class foo {
                typedef int sometype;
        };

        struct die : public foo::sometype {
        };
