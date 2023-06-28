pub fn square(num: i32) -> i32 {
    r#num * num
}

pub fn kimchi() -> i32 {
    // UTF-8 raw indentifiers
    let r#김치 = 1;
    let r#泡菜 = 1;
    let r#кимчи = 1;
    r#김치 + r#泡菜 + r#кимчи
}
