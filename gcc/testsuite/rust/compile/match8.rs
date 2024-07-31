union MyUnion {
    f1: u32,
    f2: f32,
}

fn f(u: MyUnion) -> i32 {
    unsafe {
        match u {
            MyUnion { f1: 10 } => 0,
            MyUnion { f2 } => 0,
            MyUnion { f1: 10, f2: 10.0 } => 0, // { dg-error "union patterns should have exactly one field" "" }
            MyUnion {} => 0, // { dg-error "union patterns should have exactly one field" "" }
            MyUnion { f1: () } => 0, // { dg-error "expected u32, found tuple" "" }
            _ => 1,
        }
    }
}

fn main() {}
