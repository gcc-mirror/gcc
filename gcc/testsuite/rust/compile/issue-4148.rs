// TODO: all `xfail` conditions should be changed to `target` once the ICE in #4148 is resolved

pub fn ret_parens(x: i32) -> i32 {
    // { dg-warning "unnecessary parentheses around block return value" "#4148" { xfail *-*-* } .+1 }
    ((x+1))
}

// { dg-warning "unnecessary parentheses around type" "#4148" { xfail *-*-* } .+1 }
// { dg-warning "unnecessary parentheses around pattern" "#4148" { xfail *-*-* } .+1 }
pub fn arg_ret_parens((x): (i32)) -> (i32) {
    // { dg-warning "unnecessary parentheses around block return value" "#4148" { xfail *-*-* } .+1 }
    ((x+1))
}

// { dg-warning "unnecessary parentheses around type" "#4148" { xfail *-*-* } .+1 }
pub fn ret_rpit_parens2(x: i32) -> (i32) {
    // { dg-warning "unnecessary parentheses around block return value" "#4148" { xfail *-*-* } .+1 }
    ((x+1))
}

pub fn ret_parens3(x: i32) -> i32 {
    // { dg-warning "unnecessary parentheses around block return value" "#4148" { xfail *-*-* } .+1 }
    ((x+1))
}
