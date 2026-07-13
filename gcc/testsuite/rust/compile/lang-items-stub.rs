#![feature(no_core, lang_items)]
#![no_core]


#[lang = "sized"]
pub trait Sized {}

#[lang = "future_trait"] // { dg-warning "...lang = .future_trait... is not implemented and has no effect" }
pub trait Future {
    #[lang = "poll"] // { dg-warning "...lang = .poll... is not implemented and has no effect" }
    fn poll() {}
}

#[lang = "generator"] // { dg-warning "...lang = .generator... is not implemented and has no effect" }
pub trait Generator {}

#[lang = "generator_state"] // { dg-warning "...lang = .generator_state... is not implemented and has no effect" }
pub enum GeneratorState {}

#[lang = "box_free"] // { dg-warning "...lang = .box_free... is not implemented and has no effect" }
pub fn _box_free() {}

#[repr(transparent)]
pub struct ManuallyDrop<T: ?Sized> {
    _value: T,
}

#[lang = "maybe_uninit"] // { dg-warning "...lang = .maybe_uninit... is not implemented and has no effect" }
#[repr(transparent)]
pub union MaybeUninit<T> {
    uninit: (),
    value: ManuallyDrop<T>,
}

#[lang = "drop_in_place"] // { dg-warning "...lang = .drop_in_place... is not implemented and has no effect" }
pub unsafe fn drop_in_place<T: ?Sized>(to_drop: *mut T) {}
