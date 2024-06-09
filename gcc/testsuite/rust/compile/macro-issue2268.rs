macro_rules! foo {
    ($(+ $($a:ident)*)*) => {$($($a)*)*}
}

foo!();
