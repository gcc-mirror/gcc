#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "unsize"]
pub trait Unsize<T: ?Sized> {}

#[lang = "coerce_unsized"]
pub trait CoerceUnsized<T: ?Sized> {
    // Due to our current architecture, coercion rules are hardcoded;
    // therefore, this lang item is currently non-functional.
}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>;

pub struct NonNull<T: ?Sized> {
    pub ptr: *const T,
}

pub struct Unique<T: ?Sized> {
    pub pointer: NonNull<T>,
    pub _marker: PhantomData<T>,
}

pub struct MyBox<T: ?Sized> {
    pub inner: Unique<T>,
}

pub struct TailStruct<T: ?Sized> {
    pub header: usize,
    pub data: T,
}

fn do_coercion(tail_ptr: *const TailStruct<[i32; 3]>) -> MyBox<TailStruct<[i32]>> {
    let non_null = NonNull::<TailStruct<[i32; 3]>> { ptr: tail_ptr };
    let unique = Unique::<TailStruct<[i32; 3]>> {
        pointer: non_null,
        _marker: PhantomData::<TailStruct<[i32; 3]>>,
    };
    let my_box = MyBox::<TailStruct<[i32; 3]>> { inner: unique };

    let my_box_slice: MyBox<TailStruct<[i32]>> = my_box;
    my_box_slice
}

fn clobber_stack() {
    let mut _dummy: [usize; 10] = [0xDEADBEEF; 10];
}

pub fn main() -> i32 {
    let tail: TailStruct<[i32; 3]> = TailStruct {
        header: 42,
        data: [1, 2, 3],
    };
    let tail_ptr: *const TailStruct<[i32; 3]> = &tail;

    let coerced_box = do_coercion(tail_ptr);

    clobber_stack();

    unsafe {
        let fat_ptr: *const TailStruct<[i32]> = coerced_box.inner.pointer.ptr;

        let thin_ptr = fat_ptr as *const usize;

        if *thin_ptr == 42 {
            0
        } else {
            *thin_ptr as i32
        }
    }
}
