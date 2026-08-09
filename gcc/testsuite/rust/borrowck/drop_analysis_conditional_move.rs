// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -frust-dump-bir" }
// { dg-final { scan-file bir_dump/drop_analysis_conditional_move.conditional_move.bir.dump "Drop\\(_3\\): Conditional" } }
// { dg-final { scan-file bir_dump/drop_analysis_conditional_move.static_after_join.bir.dump "Drop\\(_3\\): Static" } }
// { dg-final { scan-file bir_dump/drop_analysis_conditional_move.dead_after_join.bir.dump "Drop\\(_3\\): Dead" } }

#![feature(no_core)]
#![no_core]

fn conditional_move(condition: bool) {
    struct A {
        i: i32,
    }

    let x = A { i: 1 };

    if condition {
        let y = x;
    }
}


fn static_after_join(condition: bool) {
    struct A {
        i: i32,
    }

    let x = A { i: 1 };

    if condition {
        let y = 1;
    }
}

fn dead_after_join(condition: bool) {
    struct A {
        i: i32,
    }

    let x = A { i: 1 };

    if condition {
        let y = x;
    } else {
        let z = x;
    }
}