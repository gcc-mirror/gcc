// { dg-additional-options "-frust-cfg=A" }

macro_rules! attr {
    (#[$attr:meta] $s:stmt) => {
        #[$attr]
        $s;
    };
}

fn main() -> i32 {
    let mut a = 0;

    attr! {
    #[cfg(A)]
        a = 3
    };

    attr! {
    #[cfg(B)]
        a = 40
    };

    a - 3
}
