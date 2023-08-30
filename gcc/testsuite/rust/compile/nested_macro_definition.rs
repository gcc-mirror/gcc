// { dg-options "-frust-name-resolution-2.0" }

macro_rules! toto {
    () => {
        macro_rules! tata {
            () => {
                let _i = 0;
            };
        }
    };
}

pub fn main() {
    toto!();
    tata!();
}
