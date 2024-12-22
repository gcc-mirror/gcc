fn main(){
    let foo = 1337;
    let bar_ptr = &foo as *const i32;

    let bar_ptr_usize = bar_ptr as usize;
    let bar_ptr_isize = bar_ptr as isize;
    let bar_ptr_u64 = bar_ptr as u64;
    let bar_ptr_i64 = bar_ptr as i64;
    let bar_ptr_i8 = bar_ptr as i8;
    let bar_ptr_u8 = bar_ptr as u8;

    let _ = bar_ptr_usize as *const i32;
    let _ = bar_ptr_isize as *const i32;
    let _ = bar_ptr_u64 as *const i32;
    let _ = bar_ptr_i64 as *const i32;
    let _ = bar_ptr_i8 as *const i32;
    let _ = bar_ptr_u8 as *const i32;
}
