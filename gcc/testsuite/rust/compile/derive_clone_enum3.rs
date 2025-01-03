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
enum StructEnum {
    A { i0: i32 },
    B { i0: i32, i1: i32, i2: i32 }
}
