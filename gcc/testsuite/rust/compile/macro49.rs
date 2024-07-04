macro_rules! closure {
    () => {{
        14 + 15
    }};
}

fn main() {
    let _ = || closure!();
}
