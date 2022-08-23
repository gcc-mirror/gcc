macro_rules! stmt {
    ($s:stmt) => {
        $s
    };
    ($s:stmt, $($ss:stmt),*) => {
        $s;
        stmt!($($ss),*);
    };
}

fn main() {
    stmt!(
        struct S;
    );
    stmt!(
        struct A;,
        struct B;,
        struct C;,
        struct D;,
        struct E;
    );
}

