// { dg-additional-options "-fdump-tree-gimple" }
#![feature(no_core)]
#![no_core]

struct NonTransparent {
    foo: i32
}

#[repr(transparent)]
struct Transparent {
    foo: i32
}

fn main () -> i32 {
    // { dg-final { scan-tree-dump-times {(?n)my_obj . 42;$} 1 gimple } }
    let mut my_obj = Transparent { foo: 42 };
    // { dg-final { scan-tree-dump-times {(?n)my_obj2.foo . 40;$} 1 gimple } }
    let my_obj2 = NonTransparent { foo: 40 };
    my_obj.foo -= 2;
    my_obj.foo - my_obj2.foo
}