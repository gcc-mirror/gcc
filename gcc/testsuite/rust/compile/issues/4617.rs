#![feature(no_core)]
#![no_core]

struct Apple;

enum Delicious {
    ApplePie = Apple::PIE, // { dg-error "failed to resolve path segment using an impl Probe" }
}
