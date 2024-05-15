// This test case should error out since we haven't included the negative_impls feature
// Output from  online rust compiler 2021 ver
// Compiling playground v0.0.1 (/playground)
// error[E0658]: negative trait bounds are not yet fully implemented; use marker types for now
//  --> src/main.rs:8:6
//   |
// 8 | impl !ExampleTrait for i32 {}//
//   |      ^^^^^^^^^^^^^
//   |
//   = note: see issue #68318 <https://github.com/rust-lang/rust/issues/68318> for more information

// For more information about this error, try `rustc --explain E0658`.
// error: could not compile `playground` (bin "playground") due to 1 previous error
trait ExampleTrait {}

impl !ExampleTrait for i32 {} // { dg-error "negative_impls are not yet implemented" }