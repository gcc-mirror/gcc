macro_rules! c_fn {
    {$name:ident ($($arg_name:ident $arg_ty:ty),*) -> $ret_ty:ty} => {
        fn $name($($arg_name: $arg_ty)*) -> $ret_ty;
    };
}

extern "C" {
    c_fn! {puts (s *const i8) -> i64}
}
