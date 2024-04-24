// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

pub fn position_dependent_outlives<'a>(x: &'a mut i32, cond: bool) -> &'a mut i32 {
    let y = &mut *x;
    if cond {
        return y;
    } else {
        *x = 0;
        return x;
    }
}