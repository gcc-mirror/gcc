mod frob {}

use foo::bar::baz; // { dg-error "cannot find simple path segment .foo." }
use frob::ulator; // { dg-error "cannot find simple path segment .ulator." }

mod sain {
    mod doux {}

    mod dron {}
}

use not_sain::*; // { dg-error "cannot find simple path segment .not_sain." }

use sain::*;
use sain::{doux, dron};
use sain::{doux, dron, graal}; // { dg-error "cannot find simple path segment .graal." }
