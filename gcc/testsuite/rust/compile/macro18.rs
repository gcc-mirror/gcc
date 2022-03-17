// { dg-additional-options "-w" }

macro_rules! take_stmt {
    ($s:stmt) => {
        $s;
    };
}

fn main() -> i32 {
    take_stmt!(let complete = 15;);
    take_stmt!(let lacking = 14);

    0
}
