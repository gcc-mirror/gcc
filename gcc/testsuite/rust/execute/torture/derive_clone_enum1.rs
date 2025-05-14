#[lang = "clone"]
trait Clone {
    pub fn clone(&self) -> Self;
}

impl Clone for i32 {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone)]
enum MixAndMatch {
    A,
    B(i32),
    C { inner: i32 }
}

fn main() -> i32 {
    let a = MixAndMatch::A;
    let a_copy = a.clone();

    // we want res to stay at zero - when we don't match on the right thing, increase it

    let mut res = match a_copy {
        MixAndMatch::A => 0,
        _ => 1,
    };

    let a = MixAndMatch::B(15);
    let a_copy = a.clone();

    match a_copy {
        MixAndMatch::B(15) => {},
        _ => res += 1,
    };

    let a = MixAndMatch::C { inner: 15 };
    let a_copy = a.clone();

    match a_copy {
        MixAndMatch::C { inner } => {
            if inner != 15 {
                res += 1;
            }
        },
        _ => res += 1,
    };

    res
}
