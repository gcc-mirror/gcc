static FOO: i32 = 0;

pub fn bar() -> i32 {
    FOO
}

pub fn baz() -> i32 {
    static QUX: i32 = 0;
    QUX
}
