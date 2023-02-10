pub enum Either<L, R> {
    Left(L),
    Right(R),
}

pub struct Wrap<T>(T);

pub fn foo_wrap() -> Either<(), Wrap<u8>> {
    Either::Left(())
}
