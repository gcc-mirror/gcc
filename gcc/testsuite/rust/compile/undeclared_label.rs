// ErrorCode::E0426
#![allow(unused)]
fn resolve_label_continue() -> () {
    loop {
        continue 'a; // { dg-error "use of undeclared label .a. in .continue." }
    }
}
fn resolve_label_break() -> () {
    loop {
        break 'crabby; // { dg-error "use of undeclared label .crabby. in .break." }
    }
}
fn main() {
    resolve_label_continue();
    resolve_label_break();
}
