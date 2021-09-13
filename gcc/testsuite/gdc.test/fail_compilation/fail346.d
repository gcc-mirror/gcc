/*
TEST_OUTPUT:
---
fail_compilation/fail346.d(15): Error: undefined identifier `P`
fail_compilation/fail346.d(15): Error: variable `fail346.S.T!0.T` cannot use template to add field to aggregate `S`
fail_compilation/fail346.d(20): Error: template instance `fail346.S.T!0` error instantiating
fail_compilation/fail346.d(23):        instantiated from here: `V!(S, 0)`
---
*/

struct S {
    int x;

    template T(int val) {
        const P T = { val }; // the P here is an error it should be S
    }
}

template V(R,int val){
    const R V=R.T!(val);
}

const S x = V!(S,0);

