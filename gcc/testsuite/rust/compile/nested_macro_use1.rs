#[macro_use]
mod foo {
    #[macro_use]
    mod zim {
        #[macro_use]
        mod zoom {
            #[macro_use]
            mod zum {
                macro_rules! qux {
                    () => {};
                }
            }
        }
    }
}

fn main() {
    qux!(); // OK
}
