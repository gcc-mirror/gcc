pub fn main() {
    struct O(i32);
    struct T(i32, i32);
    struct M(i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32);

    // tuples
    let z = ();
    let o = (0,);
    let f = o.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let t = (0, 1);
    let s = t.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let m = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    let l = m.10;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    // tuple structs
    let so = O(0);
    let sf = so.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let st = T(0, 1);
    let fs = st.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let sm = M(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    let sl = sm.10;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    z
}
