// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

fn test_move_conditional(b1: bool, b2:bool) {
    struct A {
        i: i32,
    }

    let a = A { i: 1 };
    let b = a;
    if b1 {
        let b = a;
        // { dg-error "use of moved value" "" { target *-*-* } .-1 }
    /*
     { dg-begin-multiline-output "" }
   NN |     let b = a;
      |             ~    
      |             |
      |             value moved here
   NN |     if b1 {
   NN |         let b = a;
      |                 ~
      |                 |
      |                 value moved here
......
   NN |         let c = a;
      |                 ^
      |                 |
      |                 moved value used here
     { dg-end-multiline-output "" }
     */
    }
    if b2 {
        let c = a;
        // { dg-error "use of moved value" "" { target *-*-* } .-1 }
    /*
     { dg-begin-multiline-output "" }
   NN |     let b = a;
      |             ~    
      |             |
      |             value moved here
   NN |     if b1 {
   NN |         let b = a;
      |                 ^
      |                 |
      |                 moved value used here
......
   NN |         let c = a;
      |                 ~
      |                 |
      |                 value moved here
     { dg-end-multiline-output "" }
     */
    }
}

fn test_move_fixed(b1: bool, b2:bool) {

    let a = 1; // a is now primitive and can be copied
    let b = a;
    if b1 {
        let b = a;
    }
    if b2 {
        let c = a;
    }
}
