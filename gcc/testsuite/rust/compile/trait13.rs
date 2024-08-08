// Testing multiple supertraits and calling supertrait methods

struct Foo {
    my_int: u32,
}

trait GrandParent {
    fn grandparent(&self) -> u32;
}

trait Parent : GrandParent {
    fn parent(&self) -> bool;
}

trait Child : Parent {
    fn child(&self);
}

impl GrandParent for Foo {
    fn grandparent(&self) -> u32 {
        self.my_int
    }
}

impl Parent for Foo {
    fn parent(&self) -> bool {
        // Call supertrait method
        return self.grandparent() != 0;
    }
}

impl Child for Foo {
    fn child(&self) {
        let _ = self;
    }
}

pub fn main() {
    let a = Foo{my_int: 0xfeedf00d};
    let b: &dyn Child = &a;

    b.parent();
    b.child();

    // Here to silence bogus compiler warning
    let _ = a.my_int;
}
