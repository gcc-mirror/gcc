// { dg-options "-w" }
fn main(){
    let foo:f64 = 13.37;
    let _ = foo as i64;
    let _ = foo as u64;
    let _ = foo as isize;
    let _ = foo as usize;
    let _ = foo as i8;
    let _ = foo as u8;
}
