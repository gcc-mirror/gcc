// { dg-additional-options "-frust-unused-check-2.0" }

pub fn foo_1() {
    'a: loop {
        break 'a;
    }
}

pub fn foo_2() {
    'a: loop {
// { dg-warning "unused label ..a." "" { target *-*-* } .-1 }
        break;
    }
}


pub fn bar_1() {
    'a: loop {
        continue 'a;
    }
}

pub fn bar_2() {
    'a: loop {
// { dg-warning "unused label ..a." "" { target *-*-* } .-1 }
        continue;
    }
}
