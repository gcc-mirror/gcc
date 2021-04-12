fn test1() -> i32 {
    1
}

fn test2() -> i32 {
    return 2;
}

fn test3(x: i32) -> i32 {
    if x > 1 {
        5
    } else {
        0
    }
}

fn test4(x: i32) -> i32 {
    if x > 1 {
        return 1;
    }
    0
}

fn test5(x: i32) -> i32 {
    if x > 1 {
        if x == 5 {
            7
        } else {
            9
        }
    } else {
        0
    }
}

fn test6(x: i32) -> i32 {
    if x > 1 {
        return 5;
    } else {
        return 0;
    }
}

fn test7(x: i32) -> i32 {
    if x > 1 {
        return 5;
    } else {
        return 0;
    }
}

fn test8() -> i32 {
    return 1;
}

fn main() {
    let call1 = test1();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call2 = test2();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call3 = test3(3);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call4 = test4(4);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call5 = test5(5);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call6 = test6(6);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call7 = test7(7);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call8 = test8();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
