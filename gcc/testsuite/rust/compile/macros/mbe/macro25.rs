macro_rules! valid {
    ($($a:literal)* $i:ident) => {{}};
}

fn main() {
    valid!(1 one_lit);
    valid!(identifier_only);
    valid!(1 2 two_lits);
}
