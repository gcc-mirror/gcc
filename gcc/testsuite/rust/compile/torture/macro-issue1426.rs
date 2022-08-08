// { dg-additional-options -fdump-tree-ccp1-raw }

macro_rules! stmt {
    ($s:stmt) => {
        $s
    };
    ($s:stmt, $($ss:stmt),*) => {
        $s;
        stmt!($($ss),*);
    };
}

pub fn test() -> i32 {
    stmt!(
        let a = 1
	// { dg-warning {unused name 'a'} {} { target *-*-* } .-1 }
    );
    stmt!(
        let b = 2,
        let c = 3,
        let d = 4,
        let e = 5,
        let f = b + c + d + e
    );
    f
    // { dg-final { scan-tree-dump-times {gimple_return <14>} 1 ccp1 { target __OPTIMIZE__ } } }
}

fn main() {
    let _ = test();
}

