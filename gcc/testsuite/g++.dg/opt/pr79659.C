// PR target/79659
// { dg-do compile }
// { dg-options "-flifetime-dse=123456" }

// { dg-error "is not between 0 and 2" "" { target *-*-* } 0 }
