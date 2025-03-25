impl B for u32 {
    const BAR: i32; // { dg-error "associated constant in .impl." }
}

trait B {}
