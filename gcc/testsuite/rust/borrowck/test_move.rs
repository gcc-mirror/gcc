// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

fn test_move() {
    struct A {
        i: i32,
    }
    let a = A { i: 1 };
    let b = a;
    let c = a; //~ ERROR
    // { dg-error "use of moved value" "" { target *-*-* } .-1 }
    /*
     { dg-begin-multiline-output "" }
   NN |     let b = a;
      |             ~
      |             |
      |             value moved here
   NN |     let c = a; //~ ERROR
      |             ^
      |             |
      |             moved value used here
     { dg-end-multiline-output "" }
     */
    
}

fn test_move_fixed() {

    let a = 1; // a is now primitive and can be copied
    let b = a;
    let c = a;
}
