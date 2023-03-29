// { dg-additional-options "-w" }

macro_rules! take_stmt {
    ($s:stmt) => {
        $s;
    };
}

fn main() -> i32 {
    take_stmt!(let complete = 15;); // { dg-error "Failed to match any rule within macro" }
    take_stmt!(let lacking = 14);

    0
}
