macro_rules! foo {
    ($(+)+) => {e};
    () => {}
}

foo!();
