pub struct A<T>(T);
pub struct B<T>(T);

pub fn foo(_: A<B<i32>>) {}
