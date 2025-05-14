macro_rules! repeat {
    ( $( $i:literal ),* ; $( $j:literal ),* ) => (( $( ($i,$j) ),* ))
    // { dg-error "different amount of matches used in merged repetitions" "" { target *-*-* } .-1 }
}

fn main() -> i32 {
    let _ = repeat!(1, 2, 3; 2, 3);

    0
}
