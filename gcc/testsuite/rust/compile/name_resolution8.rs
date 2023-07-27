// { dg-options "-frust-name-resolution-2.0" }

// check that macros by example get exported to the crate's root with #[macro_export]
pub mod foo {
    pub mod bar {
        pub mod baz {
            pub mod qux {
				#[macro_export]
                macro_rules! foo {
                    (one) => {};
                }
            }
        }
    }
}

crate::foo!(one); // ok
foo!(one); // ok

mod a {
	mod b {
		mod c {
			super::super::super::foo!(one); // ok
		}
	}
}
