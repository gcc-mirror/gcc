#[allow(path_statements)]

macro_rules! array_impl_default {
    {$t:ident} => {
        $t;
        array_impl_default!{}
    };
    {} => {}
}

pub fn foo() {
    let x = 12;
    array_impl_default! {x}
}
