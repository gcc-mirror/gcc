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
enum AllIdentifiers {
    A,
    B
}
