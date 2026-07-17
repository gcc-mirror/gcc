#![feature(no_core, lang_items, optin_builtin_traits)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "unsize"]
pub trait Unsize<T: ?Sized> {}

fn unsize_array<T: ?Sized>() where T: Unsize<[i32]> {}
fn unsize_trait<T: ?Sized>() where T: Unsize<dyn Animal> {}
fn unsize_dyn<T: ?Sized>() where T: Unsize<dyn Animal> {}
fn unsize_struct<T: ?Sized>() where T: Unsize<_MyWrapper<[i32]>> {}
fn unsize_tuple<T: ?Sized>() where T: Unsize<(i32, [i32])> {}

trait Animal {}
struct _Dog;
impl Animal for _Dog {}
auto trait Send {}

struct _MyWrapper<T: ?Sized> {
    pub data: T,
}

fn main() {
    // 1. ARRAY -> SLICE
    unsize_array::<[i32; 3]>();

    // 2. T -> dyn Trait
    unsize_trait::<_Dog>();

    // 3. Dynamic -> Dynamic
    unsize_dyn::<dyn Animal + Send>();

    // 4. Struct -> Struct
    unsize_struct::<_MyWrapper<[i32; 3]>>();

    // 5. Tuple -> Tuple
    unsize_tuple::<(i32, [i32; 3])>();
}
