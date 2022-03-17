// { dg-additional-options "-w" }

fn foo(e: &str) -> &str { // { dg-bogus "expected" "#391" { xfail *-*-* } }
    &"" // { dg-bogus "expected" "#391" { xfail *-*-* } }
}
