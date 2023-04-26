fn foo() -> i32 {
    {54}
    #[cfg(all(A, not(A)))]
    {45}
}

fn main() -> i32 {
    return foo() - 54;
}
