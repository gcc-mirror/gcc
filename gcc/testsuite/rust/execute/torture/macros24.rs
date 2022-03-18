macro_rules! repeat {
    ( $( $i:literal ),* ; $( $j:literal ),* ) => (( $( ($i,$j) ),* ))
}

fn main() -> i32 {
    let t = repeat!(1, 1; 2, 2);

    t.0 .0 + t.0 .1 + t.1 .0 + t.1 .1 - 6
}
