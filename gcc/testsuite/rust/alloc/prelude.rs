pub use crate::core;
use crate::{
    assert, assert_eq, cfg, debug_assert, debug_assert_eq, debug_assert_ne, format_args, panic,
    unreachable, write,
};
use core::alloc::AllocError;
use core::clone::Clone;
use core::cmp::{Eq, Ord, PartialEq, PartialOrd};
use core::convert::{AsMut, AsRef, From};
use core::default::Default;
use core::fmt::Debug;
use core::hash::Hash;
use core::iter::{DoubleEndedIterator, ExactSizeIterator, Extend, IntoIterator, Iterator};
use core::marker::{Copy, Send, Sized, Sync};
use core::mem::drop;
use core::ops::{Drop, Fn, FnMut, FnOnce};
use core::option::Option::{self, None, Some};
use core::result::Result::{self, Err, Ok};
use core::str::Chars;
