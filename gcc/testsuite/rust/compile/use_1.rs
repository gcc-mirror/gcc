// { dg-additional-options "-frust-name-resolution-2.0" }
mod frob {}

use foo::bar::baz; // { dg-error "unresolved import .foo::bar::baz." }
use frob::ulator; // { dg-error "unresolved import .frob::ulator." }

mod sain {
    mod doux {}

    mod dron {}
}

use not_sain::*; // { dg-error "unresolved import .not_sain." }

use sain::*;
use sain::{doux, dron};
use sain::{doux, dron, graal}; // { dg-error "unresolved import .sain::graal." }
