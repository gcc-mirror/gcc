// { dg-additional-options "-frust-unused-check-2.0" }
pub fn a()->i32 {
    let mut a = 1;
    a = 2;
// { dg-warning "unused assignment .a." "" { target *-*-* } .-1 }
    a = 3;
// { dg-warning "unused assignment .a." "" { target *-*-* } .-1 }
    a = 4;
// { dg-warning "unused assignment .a." "" { target *-*-* } .-1 }
    a = 5;
    let mut b = a;
    b = 1;
// { dg-warning "unused assignment .b." "" { target *-*-* } .-1 }
    b = 2;
// { dg-warning "unused assignment .b." "" { target *-*-* } .-1 }
    b = 3;
// { dg-warning "unused assignment .b." "" { target *-*-* } .-1 }
    b = 4;
// { dg-warning "unused assignment .b." "" { target *-*-* } .-1 }
    b = 5;
    return b
}
