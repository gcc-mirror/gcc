// { dg-additional-options "-frust-crate-type=proc-macro" }

mod inner {
    struct InnerStruct;
}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
type AliasedType = inner::InnerStruct;

// { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" "" { target *-*-* } .+1 }
#[proc_macro]
use inner::InnerStruct;

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
struct MyStruct;

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
struct MyCurlyStruct {
    member: usize,
}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
struct MyTupleStruct(usize);

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
extern crate my_extern_crate; // { dg-error "unknown crate .my_extern_crate." }
                              // { dg-error "failed to locate crate .my_extern_crate." "" { target *-*-* } .-1 }

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
mod my_module {}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
enum MyEnum {}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
union MyUnion {
    f1: u32,
    f2: f32,
}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
const MY_CONST_STR: &str = "my_string";

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
static MY_STATIC_USIZE: usize = 10;

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
trait MyTrait {}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
impl MyStruct {}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
impl MyTrait for MyStruct {}

#[proc_macro] // { dg-error "the .#.proc_macro.. attribute may only be used on bare functions" }
extern "C" {}
