macro_rules! foo {
    // { dg-error "does not take a separator" "#2092" { target *-*-*} .+1 }
    ($(a),?) => {};
}
