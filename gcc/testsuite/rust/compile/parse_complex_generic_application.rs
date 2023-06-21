pub enum Either<T, E> {
    Left(T),
    Right(E),
}

pub mod err {
    pub struct Error;
    pub struct ErrorWrap<T>(T);
}

pub fn foo_err() -> Either<(), err::Error> {
    Either::Left(())
}

pub fn foo_err_wrap() -> Either<(), err::ErrorWrap<u8>> {
    Either::Left(())
}
