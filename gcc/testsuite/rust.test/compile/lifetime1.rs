fn foo<'a>(t: &'a str) -> &'a str {
    t
}

fn main() {
    foo("hello world");
}
