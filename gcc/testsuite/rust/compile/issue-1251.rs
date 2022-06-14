// { dg-additional-options "-w" }
mod a {
    pub mod b {
        pub mod a {
            pub fn foo() {}
        }
    }

    pub fn bidule() {
        crate::a::b::a::foo()
    }
}

fn main() {}
