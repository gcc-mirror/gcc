/// Contains the core of the Polonius borrow checking engine.
/// Input is fed in via AllFacts, and outputs are returned via Output
extern crate datafrog;
#[macro_use]
extern crate log;
extern crate rustc_hash;

mod facts;
mod output;

// Reexports of facts
pub use facts::AllFacts;
pub use facts::Atom;
pub use facts::FactTypes;
pub use output::Algorithm;
pub use output::Output;
