#[lang = "clone"]
trait Clone {
    fn clone(&self) -> Self;

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

#[lang = "copy"]
pub trait Copy: Clone {
    // Empty.
}

mod impls {
    use super::Clone;

    impl Clone for char {
        fn clone(&self) -> Self {
            *self
        }
    }
}
