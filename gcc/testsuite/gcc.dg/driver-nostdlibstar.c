// Test that the GCC driver (which has no concept of libstdc++) rejects -nostdlib++
// { dg-additional-options "-nostdlib++" }
// { dg-prune-output "compilation terminated" }
// { dg-error "unrecognized command-line option '-nostdlib\\+\\+'" "" { target *-*-* } 0 }
