// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

fn missing_subset<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 {
    // { dg-error "Found subset errors in function missing_subset" "" { target *-*-* } .-1 }
    y //~ ERROR
}

fn missing_subset_fixed<'a, 'b>(x: &'a u32, y: &'b u32) -> &'a u32 where 'b: 'a {
    y
}

fn complex_cfg_subset<'a, 'b>(b: bool, x: &'a u32, y: &'b u32) -> &'a u32 {
    // { dg-error "Found subset errors in function complex_cfg_subset" "" { target *-*-* } .-1 }
    if b {
        y //~ ERROR
    } else {
        x
    }
}

fn complex_cfg_subset_fixed<'a, 'b>(b: bool, x: &'a u32, y: &'b u32) -> &'a u32 where 'b: 'a {
    if b {
        x
    } else {
        y
    }
}