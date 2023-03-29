// { dg-additional-options "-w -fdump-tree-gimple -frust-crate=example" }
struct Foo(i32);

trait TR {
    fn test(&self) -> i32;
}

mod A {
    impl ::Foo {
        pub fn test(self) {}
        // { dg-final { scan-tree-dump-times {example::A::<impl example::Foo>::test} 2 gimple } }
    }

    impl ::TR for ::Foo {
        fn test(&self) -> i32 {
            // { dg-final { scan-tree-dump-times {example::A::<impl example::Foo as example::TR>::test} 1 gimple } }
            self.0
        }
    }
}

pub fn test() {
    let a = Foo(123);
    a.test();
}
