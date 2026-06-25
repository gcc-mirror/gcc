#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

fn main() {
    let arr: [i32; 4] = [1, 2, 3, 4];
    let mut arr_mut: [i32; 4] = [1, 2, 3, 4];

    // 1. &mut T -> &mut U
    let _mut_to_mut: &mut [i32] = &mut arr_mut;
    
    // 2. &mut T -> &U
    let _mut_to_ref: &[i32] = &mut arr_mut;
    
    // 3. &mut T -> *mut U
    let _mut_to_ptr_mut: *mut [i32] = &mut arr_mut;
    
    // 4. &mut T -> *const U
    let _mut_to_ptr_const: *const [i32] = &mut arr_mut;

    // 5. &T -> &U
    let _ref_to_ref: &[i32] = &arr;
    
    // 6. &T -> *const U
    let _ref_to_ptr_const: *const [i32] = &arr;

    // 7. *mut T -> *mut U
    let raw_mut: *mut [i32; 4] = &mut arr_mut;
    let _ptr_mut_to_ptr_mut: *mut [i32] = raw_mut;

    // 8. *mut T -> *const U
    let _ptr_mut_to_ptr_const: *const [i32] = raw_mut;

    // 9. *const T -> *const U
    let raw_const: *const [i32; 4] = &arr;
    let _ptr_const_to_ptr_const: *const [i32] = raw_const;
}
