macro_rules! a {
    (1) => {x};
    (2) => {};
}

macro_rules! b {
    (a) => {x};
    (b) => {};
}

a!(2);
b!(b);
