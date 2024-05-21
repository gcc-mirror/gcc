// { dg-options "-frust-compile-until=lowering" }

fn test(n: usize) {
    match n {
        0..10 => (), //{ dg-error "exclusive range pattern syntax is experimental." "" { target *-*-* }  }
        _ => (),
    }
}
