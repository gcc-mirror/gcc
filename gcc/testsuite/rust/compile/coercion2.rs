#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>;

#[lang = "coerce_unsized"]
pub trait CoerceUnsized<T: ?Sized> {
    // This lang item is not used for now.
}

pub struct NonNull<T: ?Sized> {
    _ptr: *const T,
}

pub struct Unique<T: ?Sized> {
    _pointer: NonNull<T>,
    _marker: PhantomData<T>,
}

pub struct MyBox<T: ?Sized> {
    _inner: Unique<T>,
}

pub struct TailStruct<T: ?Sized> {
    _header: usize,
    _data: T,
}

pub fn test_nested_coercions() {
    let tail: TailStruct<[i32; 3]> = TailStruct { _header: 1, _data: [1, 2, 3] };
    let tail_ptr: *const TailStruct<[i32; 3]> = &tail;
    
    let non_null = NonNull::<TailStruct<[i32; 3]>> { _ptr: tail_ptr };
    
    let unique = Unique::<TailStruct<[i32; 3]>> { 
        _pointer: non_null, 
        _marker: PhantomData::<TailStruct<[i32; 3]>> 
    };
    
    let my_box = MyBox::<TailStruct<[i32; 3]>> { _inner: unique };

    let _my_box_slice: MyBox<TailStruct<[i32]>> = my_box;
}
