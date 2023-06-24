macro_rules! id {
    ($i:ident) => { $i }
}

pub enum F {
    Tuple(id!(u32)),
    Struct { field: id!(u64) },
}

fn main() {}
