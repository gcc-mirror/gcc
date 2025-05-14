// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" }
// { dg-enable-nn-line-numbers "" }

fn missing_subset<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 {
    // { dg-error "subset error, some lifetime constraints need to be added" "" { target *-*-* } .-1 }
    y //~ ERROR
    /*
     { dg-begin-multiline-output "" }
   NN | fn missing_subset<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 {
      | ^~                ~~  ~~
      | |                 |   |
      | |                 |   lifetime defined here
      | |                 lifetime defined here
      | subset error occurs in this function
     { dg-end-multiline-output "" }
    */
}

fn missing_subset_fixed<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 where 'b: 'a {
    y
}

fn complex_cfg_subset<'a, 'b>(b: bool, x: &'a u32, y: &'b u32) -> &'a u32 {
    // { dg-error "subset error, some lifetime constraints need to be added" "" { target *-*-* } .-1 }
    if b {
        y //~ ERROR
    } else {
        x
    }
    /*
     { dg-begin-multiline-output "" }
   NN | fn complex_cfg_subset<'a, 'b>(b: bool, x: &'a u32, y: &'b u32) -> &'a u32 {
      | ^~                    ~~  ~~
      | |                     |   |
      | |                     |   lifetime defined here
      | |                     lifetime defined here
      | subset error occurs in this function
     { dg-end-multiline-output "" }
    */
}

fn complex_cfg_subset_fixed<'a, 'b>(b: bool, x: &'a u32, y: &'b u32) -> &'a u32 where 'b: 'a {
    if b {
        x
    } else {
        y
    }
}
