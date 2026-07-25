// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck -frust-dump-bir" }
// { dg-final { scan-file bir_dump/drop_analysis_whole_move.static_local.bir.dump "Drop\\(_2\\): Static" } }
// { dg-final { scan-file bir_dump/drop_analysis_whole_move.whole_move.bir.dump "Drop\\(_4\\): Static" } }
// { dg-final { scan-file bir_dump/drop_analysis_whole_move.whole_move.bir.dump "Drop\\(_2\\): Dead" } }
// { dg-final { scan-file bir_dump/drop_analysis_whole_move.copy.bir.dump "Drop\\(_4\\): Static" } }
// { dg-final { scan-file bir_dump/drop_analysis_whole_move.copy.bir.dump "Drop\\(_2\\): Static" } }
// { dg-final { scan-file bir_dump/drop_analysis_whole_move.function_argument.bir.dump "Drop\\(_2\\): Static" } }

#![feature(no_core)]
#![no_core]

fn static_local() {
    struct A {
        i: i32,
    }

    let x = A { i: 1 };
}

fn whole_move() {
    struct A {
        i: i32,
    }

    let x = A { i: 1 };
    let y = x;
}

fn copy() {
    let x = 1;
    let y = x;
}

fn function_argument(x: i32) {}
