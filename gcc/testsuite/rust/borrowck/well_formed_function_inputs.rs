// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

fn foo<'a, 'b>(p: &'b &'a mut usize) -> &'b&'a mut usize {
    p
}

fn well_formed_function_inputs() {
    let s = &mut 1;
    let r = &mut *s;
    let tmp = foo(&r  );
    // let arg = &r;
    // let aarg = &*arg;
    // let tmp = arg;
    s; //~ ERROR
    // { dg-error "use of borrowed value" "" { target *-*-* } .-1 }
    tmp;
    /*
     { dg-begin-multiline-output "" }
   NN |     let r = &mut *s;
      |             ~
      |             |
      |             borrow occurs here
......
   NN |     s; //~ ERROR
      |     ^
      |     |
      |     borrowed value used here
     { dg-end-multiline-output "" }
    */
}
