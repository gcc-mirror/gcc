// { dg-require-effective-target lp64 }
#![feature(no_core, lang_items, intrinsics)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "coerce_unsized"]
pub trait CoerceUnsized<T: ?Sized> {
    // Due to our current architecture, coercion rules are hardcoded;
    // therefore, this lang item is currently non-functional.
}

extern "rust-intrinsic" {
    fn size_of_val<T: ?Sized>(_: *const T) -> usize;
}

pub struct TailStruct<T: ?Sized> {
    pub a: i32,
    pub tail: T,
}

pub fn _coerce(s: &TailStruct<[i32; 3]>) -> &TailStruct<[i32]> {
    s as &TailStruct<[i32]>
}

pub trait MyTrait {
    fn dummy(&self) -> i32;
}

impl MyTrait for i64 {
    fn dummy(&self) -> i32 {
        0
    }
}

impl MyTrait for [i32; 3] {
    fn dummy(&self) -> i32 {
        0
    }
}

pub fn sovt(s: &TailStruct<dyn MyTrait>) -> usize {
    unsafe { size_of_val(s as *const TailStruct<dyn MyTrait>) }
}

pub fn sov1(s: &[i32]) -> usize {
    unsafe { size_of_val(s as *const [i32]) }
}

pub fn sov2(s: &TailStruct<[i32]>) -> usize {
    unsafe { size_of_val(s as *const TailStruct<[i32]>) }
}

pub fn sov3(s: &TailStruct<TailStruct<[i32]>>) -> usize {
    unsafe { size_of_val(s as *const TailStruct<TailStruct<[i32]>>) }
}

fn main() -> i32 {
    let t = [1, 2, 3];
    let t1 = &t as &[i32];
    let s1 : TailStruct<[i32; 3]> = TailStruct {
        a: 10,
        tail: t,
    };
    let s2_tail: TailStruct<[i32; 3]> = TailStruct {
        a: 10,
        tail: t,
    };
    let s2 : TailStruct<TailStruct<[i32; 3]>> = TailStruct { a: 20, tail: s2_tail };

    let a = sov1(t1);
    let b = sov2(&s1);
    let c = sov3(&s2);
    let d = sov1(&s1.tail);
    let e = sov2(&s2.tail);

    let s3 = TailStruct {
        a: 10,
        tail: 10_i64,
    };
    let s4 = TailStruct { a: 20, tail: t };

    let r1: &TailStruct<dyn MyTrait> = &s3;
    let r2: &TailStruct<dyn MyTrait> = &s4;

    let f = sovt(r1);
    let g = sovt(r2);

    let slice_ok = a == 12 && b == 16 && c == 20 && d == a && e == b;
    let trait_ok = f == 12 && g == 16;

    if slice_ok && trait_ok {
        0
    } else {
        1
    }
}
