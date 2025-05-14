macro_rules! foo {
    ([]) => {struct Foo;};
    (()) => {struct _A;};
    (bool) => {struct _B;};
}

foo! (());
foo! (bool);
