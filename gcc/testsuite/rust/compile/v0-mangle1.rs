// { dg-additional-options -frust-mangling=v0 }
#[lang = "sized"]
pub trait Sized {}

pub fn foo() {}

pub mod module_a {
    pub fn bar() {}

    pub mod module_b {
        pub fn baz() {}
    }
}

struct S; // { dg-warning "struct is never constructed" }

// name starting with underscore.
pub fn _uc() {}

pub fn generic1<T>() {}

pub fn generic2<T, U, V>() {}

pub fn 初音ミク() {}

pub fn іржа() {}

pub fn あ1() {}

fn main() {
    // { dg-final { scan-assembler "_R.*NvC.*10v0_mangle13foo" } }
    // cf. rustc 1.72.0: _RNvCshIBIgX6Bzox_10v0_mangle13foo
    foo();

    // { dg-final { scan-assembler "_R.*NvNtC.*10v0_mangle18module_a3bar" } }
    // cf. rustc 1.72.0: _RNvNtCshIBIgX6Bzox_10v0_mangle18module_a3bar
    module_a::bar();

    // { dg-final { scan-assembler "_R.*NvNtNtC10v0_mangle18module_a8module_b3baz" } }
    // cf. rustc 1.72.0: _RNvNtNtCshIBIgX6Bzox_10v0_mangle18module_a8module_b3baz
    module_a::module_b::baz();

    // { dg-final { scan-assembler "_R.*NvC.*10v0_mangle13__uc" } }
    // cf. rustc 1.72.0: _RNvCshIBIgX6Bzox_10v0_mangle13__uc
    _uc();

    // { dg-final { scan-assembler "_R.*INvC.*10v0_mangle18generic1lE.*" } }
    // cf. rustc 1.72.0: _RINvCshIBIgX6Bzox_10v0_mangle18generic1lEB2_
    generic1::<i32>();

    // { dg-final { scan-assembler "_R.*INvC.*10v0_mangle18generic1NtC.*10v0_mangle11SE.*" } }
    // cf. rustc 1.72.0: _RINvCshIBIgX6Bzox_10v0_mangle18generic1NtB2_1SEB2_
    generic1::<S>();

    // { dg-final { scan-assembler "_R.*INvC.*10v0_mangle18generic2hfjE.*" } }
    // cf. rustc 1.72.0: _RINvCshIBIgX6Bzox_10v0_mangle18generic2hfjEB2_
    generic2::<u8, f32, usize>();

    // { dg-final { scan-assembler "_R.*NvC.*10v0_mangle1u13pck1ew32ihn2d" } }
    // cf. rustc 1.72.0: _RNvCshIBIgX6Bzox_10v0_mangle1u13pck1ew32ihn2d
    初音ミク();

    // { dg-final { scan-assembler "_R.*NvC.*10v0_mangle1u8_80al3a6f" } }
    // cf. rustc 1.72.0: _RNvCshIBIgX6Bzox_10v0_mangle1u8_80al3a6f
    іржа();

    // { dg-final { scan-assembler "_R.*NvC.*10v0_mangle1u5_1_w7t" } }
    // cf. rustc 1.72.0: _RNvCshIBIgX6Bzox_10v0_mangle1u5_1_w7t
    あ1();
}
