pub fn myfun1((x, _): (i32, i32)) -> i32 {
    x
}

pub fn myfun2() -> i32 {
    let (x, _) = (1, 2);
    x
}
