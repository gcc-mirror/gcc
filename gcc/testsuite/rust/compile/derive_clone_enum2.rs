#[lang = "clone"]
trait Clone {
    pub fn clone(&self) -> Self;
}

impl Clone for i32 {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone)]
enum TupleEnum {
    A(i32),
    B(i32, i32, i32)
}
