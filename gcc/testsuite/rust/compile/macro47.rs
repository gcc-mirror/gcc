// Check the follow-set of :vis in macro rules.

macro_rules! my_mac {
    ($v:vis async) => {
        $v struct Foo(i32);
    };
    ($v:vis $i:ident) => {
        $v struct $i(i32);
    }
}
