// check that macros by example get exported to the crate's root with #[macro_export]
#![feature(no_core)]
#![no_core]

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
