// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

fn foo<'a, 'b>(p: &'b &'a mut usize) -> &'b&'a mut usize {
    p
}

fn well_formed_function_inputs() { // { dg-error "Found loan errors in function well_formed_function_inputs" }
    let s = &mut 1;
    let r = &mut *s;
    let tmp = foo(&r  );
    // let arg = &r;
    // let aarg = &*arg;
    // let tmp = arg;
    s; //~ ERROR
    tmp;
}