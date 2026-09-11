#![feature(no_core)]
#![no_core]
mod framing {
    mod public_message {
        pub struct ConfirmedTranscriptHashInput;
    }
    mod public_message_in {
        pub struct ConfirmedTranscriptHashInput;
    }
    pub use self::public_message::*;
    pub use self::public_message_in::*;
}

use crate::framing::ConfirmedTranscriptHashInput; // { dg-error "is ambiguous .E0659." }

fn main() {}