
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

