// ErrorCode - E0635
#![feature(AA)] //{ dg-error "unknown feature .AA." }
#![feature(iamcrabby)] // { dg-error "unknown feature .iamcrabby." }
#![feature(nonexistent_gccrs_feature)] // { dg-error "unknown feature .nonexistent_gccrs_feature." }
// ErrorCode - E0556
#![feature] // { dg-error "malformed .feature. attribute input" }

fn main() {}
