fn foo (e: &str) -> &str {
    &"" // { dg-bogus "cannot strip expression in this position - outer attributes not allowed" "#391" { xfail *-*-* } }
}
