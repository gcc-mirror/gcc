mod a {
    pub mod b {

        pub fn f(x: [u8; { 100 }]) -> [u8; { 100 }] {
            x
        }
    }
}
