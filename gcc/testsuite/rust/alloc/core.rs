/*
 * ==========  GCCRS  MOCK-CORE  ==========
 */

// #![feature(no_core, lang_items, intrinsics, rustc_attrs, optin_builtin_traits)]
// #![no_core]

// ----------  MACROS     START  ----------

#[macro_export]
macro_rules! format_args {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! assert {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! assert_eq {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! debug_assert {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! debug_assert_eq {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! debug_assert_ne {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! cfg {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! write {
    ($dst:expr, $($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! panic {
    ($($arg:tt)*) => {
        loop {}
    };
}
#[macro_export]
macro_rules! unreachable {
    ($($arg:tt)*) => {
        loop {}
    };
}

pub use crate::{
    assert, assert_eq, cfg, debug_assert, debug_assert_eq, debug_assert_ne, format_args, panic,
    unreachable, write,
};

// ----------  MACROS       END  ----------

// ----------  INTRINSICS START  ----------

pub mod intrinsics {
    extern "rust-intrinsic" {
        pub fn abort() -> !;
        pub fn assume(b: bool);
        pub fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
        pub fn min_align_of_val<T: ?super::marker::Sized>(_: *const T) -> usize;
        pub fn size_of_val<T: ?super::marker::Sized>(_: *const T) -> usize;
        pub fn size_of<T>() -> usize;
        pub fn min_align_of<T>() -> usize;
        pub fn copy<T>(src: *const T, dst: *mut T, count: usize);
        pub fn arith_offset<T>(dst: *const T, offset: isize) -> *const T;
        pub fn write_bytes<T>(dst: *mut T, val: u8, count: usize);
    }
    pub fn assert_zero_valid<T>() {}
}

// ----------  INTRINSICS   END  ----------

// ----------  USES       START  ----------

// ----------  USES         END  ----------

// ----------  MODULES    START  ----------

pub mod ops {
    //  ^^^ <- core::ops

    pub use self::arith::{Add, AddAssign, Sub};
    pub use self::deref::{Deref, DerefMut, Receiver};
    pub use bit::{BitAnd, BitOr, BitXor};
    pub use drop::Drop;
    pub use function::{Fn, FnMut, FnOnce};
    pub use generator::{Generator, GeneratorState};
    pub use index::{Index, IndexMut};
    pub use range::{Bound, Range, RangeBounds};
    pub use try::Try;
    pub use unsize::{CoerceUnsized, DispatchFromDyn};

    use super::marker::Sized;

    pub mod range {
        use crate::core::clone::Clone;
        use crate::core::cmp::PartialOrd;
        use crate::core::marker::Sized;

        #[lang = "RangeFull"]
        #[derive(Copy, Clone, Default, PartialEq, Eq, Hash)]
        pub struct RangeFull;

        #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
        pub enum Bound<T> {
            Included(T),
            Excluded(T),
            Unbounded,
        }
        #[lang = "Range"]
        #[derive(Clone, Default, PartialEq, Eq, Hash)] // not Copy -- see #27186
        pub struct Range<Idx> {
            pub start: Idx,
            pub end: Idx,
        }
        #[lang = "RangeFrom"]
        #[derive(Clone, PartialEq, Eq, Hash)] // not Copy -- see #27186
        pub struct RangeFrom<Idx> {
            pub start: Idx,
        }
        #[lang = "RangeTo"]
        #[derive(Copy, Clone, PartialEq, Eq, Hash)]
        pub struct RangeTo<Idx> {
            pub end: Idx,
        }
        #[lang = "RangeInclusive"]
        #[derive(Clone, PartialEq, Eq, Hash)] // not Copy -- see #27186
        pub struct RangeInclusive<Idx> {
            pub start: Idx,
            pub end: Idx,
            pub exhausted: bool,
        }
        impl<Idx> RangeInclusive<Idx> {
            #[lang = "range_inclusive_new"]
            #[rustc_promotable]
            pub const fn new(start: Idx, end: Idx) -> Self {
                Self {
                    start,
                    end,
                    exhausted: false,
                }
            }
            pub const fn start(&self) -> &Idx {
                &self.start
            }
            pub const fn end(&self) -> &Idx {
                &self.end
            }
            pub fn into_inner(self) -> (Idx, Idx) {
                (self.start, self.end)
            }
        }

        #[lang = "RangeToInclusive"]
        #[derive(Copy, Clone, PartialEq, Eq, Hash)]
        pub struct RangeToInclusive<Idx> {
            pub end: Idx,
        }

        pub trait RangeBounds<T: ?Sized> {
            fn start_bound(&self) -> Bound<&T>;

            fn end_bound(&self) -> Bound<&T>;

            #[track_caller]
            fn assert_len(self, len: usize) -> Range<usize>
            where
                Self: RangeBounds<usize>,
            {
                let start: Bound<&usize> = self.start_bound();
                let end: Bound<&usize> = self.end_bound();
                Range { start, end }
            }

            fn contains<U>(&self, item: &U) -> bool
            where
                T: PartialOrd<U>,
                U: ?Sized + PartialOrd<T>,
            {
                false
            }
        }
    }
    pub mod index {
        use crate::core::marker::Sized;

        #[lang = "index"]
        pub trait Index<Idx: ?Sized> {
            type Output: ?Sized;

            #[track_caller]
            fn index(&self, index: Idx) -> &Self::Output;
        }
        #[lang = "index_mut"]
        pub trait IndexMut<Idx: ?Sized>: Index<Idx> {
            #[track_caller]
            fn index_mut(&mut self, index: Idx) -> &mut Self::Output;
        }
    }
    pub mod try {
        use crate::core::result::Result;

        #[lang = "try"]
        pub trait Try {
            type Ok;
            type Error;

            #[lang = "into_result"]
            fn into_result(self) -> Result<Self::Ok, Self::Error>;

            #[lang = "from_error"]
            fn from_error(v: Self::Error) -> Self;

            #[lang = "from_ok"]
            fn from_ok(v: Self::Ok) -> Self;
        }
    }
    pub mod arith {
        #[lang = "add"]
        pub trait Add<Rhs = Self> {
            type Output;
            fn add(self, rhs: Rhs) -> Self::Output;
        }
        #[lang = "add_assign"]
        pub trait AddAssign<Rhs = Self> {
            fn add_assign(&mut self, rhs: Rhs);
        }
        #[lang = "sub"]
        pub trait Sub<Rhs = Self> {
            type Output;
            fn sub(self, rhs: Rhs) -> Self::Output;
        }
    }
    pub mod deref {
        #[lang = "deref"]
        pub trait Deref {
            type Target: ?crate::core::marker::Sized;
            fn deref(&self) -> &Self::Target;
        }

        #[lang = "deref_mut"]
        pub trait DerefMut: Deref {
            fn deref_mut(&mut self) -> &mut Self::Target;
        }

        #[lang = "receiver"]
        pub trait Receiver {
            // Empty.
        }
    }
    pub mod unsize {
        #[lang = "coerce_unsized"]
        pub trait CoerceUnsized<T: ?super::Sized> {
            // Empty.
        }

        #[lang = "dispatch_from_dyn"]
        pub trait DispatchFromDyn<T> {
            // Empty.
        }
    }
    pub mod generator {
        use crate::core::clone::Clone;
        use crate::core::pin::Pin;

        #[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
        #[lang = "generator_state"]
        pub enum GeneratorState<Y, R> {
            Yielded(Y),
            Complete(R),
        }

        #[lang = "generator"]
        #[fundamental]
        pub trait Generator<R = ()> {
            type Yield;

            type Return;

            fn resume(self: Pin<&mut Self>, arg: R) -> GeneratorState<Self::Yield, Self::Return>;
        }
    }
    pub mod function {
        #[lang = "fn_mut"]
        #[rustc_paren_sugar]
        #[fundamental]
        pub trait FnMut<Args>: FnOnce<Args> {
            extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output;
        }
        #[lang = "fn_once"]
        #[rustc_paren_sugar]
        #[fundamental]
        pub trait FnOnce<Args> {
            #[lang = "fn_once_output"]
            type Output;

            extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
        }
        #[lang = "fn"]
        #[rustc_paren_sugar]
        #[fundamental] // so that regex can rely that `&str: !FnMut`
        pub trait Fn<Args>: FnMut<Args> {
            extern "rust-call" fn call(&self, args: Args) -> Self::Output;
        }
    }
    pub mod drop {
        #[lang = "drop"]
        pub trait Drop {
            fn drop(&mut self);
        }
    }
    pub mod bit {
        #[lang = "bitand"]
        pub trait BitAnd<Rhs = Self> {
            type Output;

            fn bitand(self, rhs: Rhs) -> Self::Output;
        }
        #[lang = "bitor"]
        pub trait BitOr<Rhs = Self> {
            type Output;

            fn bitor(self, rhs: Rhs) -> Self::Output;
        }
        #[lang = "bitxor"]
        pub trait BitXor<Rhs = Self> {
            type Output;

            fn bitxor(self, rhs: Rhs) -> Self::Output;
        }
    }
}

pub mod mem {
    //  ^^^ <- core::mem
    use super::clone::Clone;
    use super::marker::Sized;

    #[lang = "manually_drop"]
    #[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct ManuallyDrop<T: ?Sized> {
        value: T,
    }

    #[inline]
    pub fn forget<T>(t: T) {
        let _ = ManuallyDrop { value: t };
    }

    #[lang = "maybe_uninit"]
    #[derive(Copy)]
    pub union MaybeUninit<T> {
        uninit: (),
        value: ManuallyDrop<T>,
    }

    #[rustc_promotable]
    pub const fn size_of<T>() -> usize {
        0
    }
    #[rustc_promotable]
    pub const fn align_of<T>() -> usize {
        0
    }
    pub const fn align_of_val<T: ?Sized>(val: &T) -> usize {
        0
    }

    pub fn drop<T>(_x: T) {}
    pub fn take<T: super::default::Default>(dest: &mut T) -> T {
        replace(dest, T::default())
    }
    pub fn replace<T>(dest: &mut T, mut src: T) -> T {
        swap(dest, &mut src);
        src
    }
    pub fn swap<T>(x: &mut T, y: &mut T) {}

    pub unsafe fn zeroed<T>() -> T {
        unsafe { T }
    }

    pub const fn size_of_val<T: ?Sized>(val: &T) -> usize {
        0
    }
    pub unsafe fn align_of_val_raw<T: ?Sized>(val: *const T) -> usize {
        0
    }
}

pub mod ptr {
    //  ^^^ <- core::ptr

    #[repr(transparent)]
    pub struct NonNull<T: ?super::marker::Sized> {
        pointer: *const T,
    }
    #[repr(transparent)]
    pub struct Unique<T: ?super::marker::Sized> {
        pointer: *const T,
        _marker: super::marker::PhantomData<T>,
    }
    #[inline]
    pub unsafe fn read<T>(src: *const T) -> T {
        loop {}
    }
    #[inline]
    pub unsafe fn write<T>(dst: *mut T, src: T) {
        let _ = (dst, src);
    }

    #[repr(C)]
    pub union Repr<T> {
        pub(crate) rust: *const [T],
        rust_mut: *mut [T],
        pub(crate) raw: FatPtr<T>,
    }
    #[repr(C)]
    pub struct FatPtr<T> {
        data: *const T,
        pub(crate) len: usize,
    }

    pub const fn slice_from_raw_parts<T>(data: *const T, len: usize) -> *const [T] {
        unsafe {
            Repr {
                raw: FatPtr { data, len },
            }
            .rust
        }
    }
    pub const fn slice_from_raw_parts_mut<T>(data: *mut T, len: usize) -> *mut [T] {
        unsafe {
            Repr {
                raw: FatPtr { data, len },
            }
            .rust_mut
        }
    }

    pub unsafe fn drop_in_place<T: ?super::marker::Sized>(to_drop: *mut T) {}

    pub unsafe fn copy<T>(src: *const T, dst: *mut T, count: usize) {}
    pub unsafe fn replace<T>(dst: *mut T, mut src: T) -> T {
        src
    }
}

pub mod marker {
    //  ^^^^^^ <- core::marker

    #[lang = "phantom_data"]
    pub struct PhantomData<T: ?super::marker::Sized>;

    #[lang = "copy"]
    pub trait Copy: super::clone::Clone {
        //    ^^^^ <- derive(Copy)
    }
    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics, derive_clone_copy)]
    pub macro Copy($item:item) {
        /* compiler built-in */
    }

    #[lang = "structural_teq"]
    pub trait StructuralEq {
        // Empty.
    }
    #[lang = "structural_peq"]
    pub trait StructuralPartialEq {
        // Empty.
    }

    #[lang = "sized"]
    #[fundamental]
    #[rustc_specialization_trait]
    pub trait Sized {
        // Empty.
    }

    #[lang = "unpin"]
    pub auto trait Unpin {}

    #[lang = "unsize"]
    pub trait Unsize<T: ?Sized> {
        // Empty.
    }

    pub unsafe auto trait Send {
        // empty.
    }
    #[lang = "sync"]
    pub unsafe auto trait Sync {
        // Empty
    }
}

pub mod cmp {
    //  ^^^ <- core::cmp

    #[derive(Clone, Copy, PartialEq, Debug, Hash)]
    pub enum Ordering {
        Less = -1,
        Equal = 0,
        Greater = 1,
    }

    #[lang = "eq"]
    pub trait PartialEq<Rhs: ?super::marker::Sized = Self> {
        //    ^^^^^^^^^ <- derive(PartialEq)
        fn eq(&self, other: &Rhs) -> bool;

        #[inline]
        fn ne(&self, other: &Rhs) -> bool {
            !self.eq(other)
        }
    }
    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics, structural_match)]
    pub macro PartialEq($item:item) {
        /* compiler built-in */
    }

    pub trait Eq: PartialEq<Self> {
        //    ^^ <- derive(Eq)
        #[inline]
        fn assert_receiver_is_total_eq(&self) {}
    }
    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics, derive_eq, structural_match)]
    pub macro Eq($item:item) {
        /* compiler built-in */
    }

    #[lang = "partial_ord"]
    pub trait PartialOrd<Rhs: ?super::marker::Sized = Self>: PartialEq<Rhs> {
        //    ^^^^^^^^^^ <- derive(PartialOrd)
        fn partial_cmp(&self, other: &Rhs) -> super::option::Option<Ordering>;

        #[inline]
        fn lt(&self, other: &Rhs) -> bool {
            false
        }

        #[inline]
        fn le(&self, other: &Rhs) -> bool {
            false
        }

        #[inline]
        fn gt(&self, other: &Rhs) -> bool {
            false
        }

        #[inline]
        fn ge(&self, other: &Rhs) -> bool {
            false
        }
    }
    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics)]
    pub macro PartialOrd($item:item) {
        /* compiler built-in */
    }

    pub trait Ord: Eq + PartialOrd<Self> {
        //    ^^^ <- derive(Ord)
        fn cmp(&self, other: &Self) -> Ordering;

        #[inline]
        fn max(self, other: Self) -> Self
        where
            Self: super::marker::Sized,
        {
            self
        }

        #[inline]
        fn min(self, other: Self) -> Self
        where
            Self: super::marker::Sized,
        {
            self
        }
        fn clamp(self, min: Self, max: Self) -> Self
        where
            Self: super::marker::Sized,
        {
            self
        }
    }

    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics)]
    pub macro Ord($item:item) {
        /* compiler built-in */
    }

    pub fn max<T: Ord>(v1: T, v2: T) -> T {
        v1.max(v2)
    }
    pub fn min<T: Ord>(v1: T, v2: T) -> T {
        v1.min(v2)
    }
}

pub mod fmt {
    //  ^^^ <- core::fmt

    pub use builders::{DebugList, DebugMap, DebugSet, DebugStruct, DebugTuple};
    pub use num::{LowerHex, UpperHex};

    use super::clone::Clone;
    use super::option::Option;

    pub struct Formatter<'a> {
        _marker: super::marker::PhantomData<&'a ()>,
    }

    pub type Result = super::result::Result<(), Error>;

    #[derive(Copy, Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub struct Error;

    pub trait Debug {
        //    ^^^^^ <- derive(Debug)
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }
    pub(crate) mod macros {
        #[rustc_builtin_macro]
        #[allow_internal_unstable(core_intrinsics)]
        pub macro Debug($item:item) {
            /* compiler built-in */
        }
    }
    pub use macros::Debug;

    pub trait Display {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }

    pub trait Pointer {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }

    #[derive(Debug)]
    pub enum Alignment {
        Left,
        Right,
        Center,
    }

    pub fn write(output: &mut dyn Write, args: Arguments<'_>) -> Result {
        Result::Ok(())
    }

    pub trait Write {
        fn write_str(&mut self, s: &str) -> Result;

        fn write_char(&mut self, c: char) -> Result {
            self.write_str(c.encode_utf8(&mut [0; 4]))
        }

        fn write_fmt(mut self: &mut Self, args: Arguments<'_>) -> Result {
            write(&mut self, args)
        }
    }

    type Opaque = usize;

    #[derive(Copy, Clone)]
    #[allow(missing_debug_implementations)]
    pub struct ArgumentV1<'a> {
        value: &'a Opaque,
        formatter: fn(&Opaque, &mut Formatter<'_>) -> Result,
    }

    #[derive(Copy, Clone)]
    pub struct Arguments<'a> {
        pieces: &'a [&'static str],
        fmt: Option<&'a [rt::v1::Argument]>,
        args: &'a [ArgumentV1<'a>],
    }

    pub trait Binary {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }
    pub trait Octal {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }
    pub trait LowerExp {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }
    pub trait UpperExp {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result;
    }

    pub mod num {
        #[derive(Clone, PartialEq)]
        struct Binary;
        #[derive(Clone, PartialEq)]
        struct Octal;
        #[derive(Clone, PartialEq)]
        struct LowerHex;
        #[derive(Clone, PartialEq)]
        struct UpperHex;
    }

    pub mod rt {
        pub mod v1 {
            #![allow(missing_debug_implementations)]

            use crate::core::clone::Clone;

            #[derive(Copy, Clone)]
            pub struct Argument {
                pub position: usize,
                pub format: FormatSpec,
            }
            #[derive(Copy, Clone)]
            pub struct FormatSpec {
                pub fill: char,
                pub align: Alignment,
                pub flags: u32,
                pub precision: Count,
                pub width: Count,
            }
            #[derive(Copy, Clone, PartialEq, Eq)]
            pub enum Alignment {
                Left,
                Right,
                Center,
                Unknown,
            }
            #[derive(Copy, Clone)]
            pub enum Count {
                Is(usize),
                Param(usize),
                Implied,
            }
        }
    }

    pub mod builders {
        use super::Formatter;

        struct PadAdapter<'buf, 'state> {
            buf: &'buf mut (dyn super::Write + 'buf),
            state: &'state mut PadAdapterState,
        }
        struct PadAdapterState {
            on_newline: bool,
        }
        #[allow(missing_debug_implementations)]
        pub struct DebugList<'a, 'b: 'a> {
            inner: DebugInner<'a, 'b>,
        }
        struct DebugInner<'a, 'b: 'a> {
            fmt: &'a mut Formatter<'b>,
            result: super::Result,
            has_fields: bool,
        }
        #[allow(missing_debug_implementations)]
        pub struct DebugMap<'a, 'b: 'a> {
            fmt: &'a mut Formatter<'b>,
            result: super::Result,
            has_fields: bool,
            has_key: bool,
            state: PadAdapterState,
        }
        #[allow(missing_debug_implementations)]
        pub struct DebugSet<'a, 'b: 'a> {
            inner: DebugInner<'a, 'b>,
        }
        #[allow(missing_debug_implementations)]
        pub struct DebugStruct<'a, 'b: 'a> {
            fmt: &'a mut Formatter<'b>,
            result: super::Result,
            has_fields: bool,
        }
        #[allow(missing_debug_implementations)]
        pub struct DebugTuple<'a, 'b: 'a> {
            fmt: &'a mut Formatter<'b>,
            result: super::Result,
            fields: usize,
            empty_name: bool,
        }
    }
}

pub mod clone {
    //  ^^^^^ <- core::clone

    #[lang = "clone"]
    pub trait Clone: super::marker::Sized {
        //    ^^^^^ <- derive(Clone)
        fn clone(&self) -> Self;

        #[inline]
        fn clone_from(&mut self, source: &Self) {
            *self = source.clone()
        }
    }
    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics, derive_clone_copy)]
    pub macro Clone($item:item) {
        /* compiler built-in */
    }
}

pub mod iter {
    //  ^^^^ <- core::iter

    pub use adapters::{zip::TrustedRandomAccess, Peekable, SourceIter};
    pub use sources::{repeat_with, RepeatWith};
    pub use traits::marker::{InPlaceIterable, TrustedLen};
    pub use traits::{
        DoubleEndedIterator, ExactSizeIterator, Extend, FromIterator, IntoIterator, Iterator,
    };

    use super::clone::Clone;
    use super::marker::Sized;
    use super::option::Option;
    use super::result::Result;

    pub trait FusedIterator {}
    pub mod traits {
        use super::Option;
        use super::Result;
        use super::Sized;
        use crate::core::ops::function::FnMut;
        use crate::core::ops::try::Try;
        pub trait IntoIterator {
            type Item;
            type IntoIter: Iterator<Item = Self::Item>;
            #[lang = "into_iter"]
            fn into_iter(self) -> Self::IntoIter;
        }

        pub trait Iterator {
            type Item;
            #[lang = "next"]
            fn next(&mut self) -> super::option::Option<Self::Item>;
        }

        pub trait FromIterator<A>: crate::core::marker::Sized {
            fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self;
        }
        pub mod marker {
            #[rustc_unsafe_specialization_marker]
            pub unsafe trait TrustedLen: super::Iterator {}
            pub unsafe trait InPlaceIterable: super::Iterator {}
        }
        pub trait DoubleEndedIterator: Iterator {
            fn next_back(&mut self) -> super::Option<Self::Item>;
            fn advance_back_by(&mut self, n: usize) -> Result<(), usize> {
                super::Result::Ok(())
            }
            fn nth_back(&mut self, n: usize) -> super::Option<Self::Item> {
                self.next_back()
            }
            fn try_rfold<B, F, R>(&mut self, init: B, mut f: F) -> R
            where
                Self: Sized,
                F: FnMut(B, Self::Item) -> R,
                R: Try<Ok = B>,
            {
                loop {}
            }
            fn rfold<B, F>(mut self, init: B, mut f: F) -> B
            where
                Self: Sized,
                F: FnMut(B, Self::Item) -> B,
            {
                let mut accum = init;
                accum
            }
            fn rfind<P>(&mut self, predicate: P) -> Option<Self::Item>
            where
                Self: Sized,
                P: FnMut(&Self::Item) -> bool,
            {
                false
            }
        }
        pub trait ExactSizeIterator: Iterator {
            fn len(&self) -> usize {
                0
            }
            fn is_empty(&self) -> bool {
                false
            }
        }
        pub trait Extend<A> {
            fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T);
            fn extend_one(&mut self, item: A) {}
            fn extend_reserve(&mut self, additional: usize) {
                let _ = additional;
            }
        }
    }
    pub mod adapters {
        use super::traits::Iterator;
        use super::Clone;
        use crate::core::option::Option;

        #[derive(Clone)]
        pub struct Map<I, F> {
            iter: I,
            f: F,
        }
        #[derive(Clone)]
        pub struct Filter<I, P> {
            iter: I,
            predicate: P,
        }
        #[derive(Clone, Debug)]
        pub struct Copied<I> {
            it: I,
        }
        pub unsafe trait SourceIter {
            type Source: Iterator;

            unsafe fn as_inner(&mut self) -> &mut Self::Source;
        }
        #[derive(Clone, Debug)]
        pub struct Peekable<I: Iterator> {
            iter: I,
            peeked: Option<Option<I::Item>>,
        }
        pub mod flatten {
            use super::fuse::Fuse;
            use super::Clone;
            use super::Map;
            use crate::core::iter::traits::{IntoIterator, Iterator};
            use crate::core::option::Option;

            pub struct Flatten<I: Iterator<Item: IntoIterator>> {
                inner: FlattenCompat<I, <I::Item as IntoIterator>::IntoIter>,
            }
            pub struct FlatMap<I, U: IntoIterator, F> {
                inner: FlattenCompat<Map<I, F>, <U as IntoIterator>::IntoIter>,
            }
            #[derive(Clone, Debug)]
            struct FlattenCompat<I, U> {
                iter: Fuse<I>,
                frontiter: Option<U>,
                backiter: Option<U>,
            }
        }
        pub mod fuse {
            use super::Clone;
            use crate::core::option::Option;

            #[derive(Clone, Debug)]
            pub struct Fuse<I> {
                iter: Option<I>,
            }
        }
        pub mod chain {
            use super::Clone;
            use crate::core::option::Option;

            #[derive(Clone, Debug)]
            pub struct Chain<A, B> {
                a: Option<A>,
                b: Option<B>,
            }
        }
        pub mod zip {
            pub unsafe trait TrustedRandomAccess: crate::core::marker::Sized {
                fn size(&self) -> usize
                where
                    Self: super::Iterator,
                {
                    0
                }
                fn may_have_side_effect() -> bool;
            }
        }
    }
    pub mod sources {
        use super::Clone;
        use crate::core::ops::function::FnMut;

        #[derive(Copy, Clone, Debug)]
        pub struct RepeatWith<F> {
            repeater: F,
        }
        pub fn repeat_with<A, F: FnMut() -> A>(repeater: F) -> RepeatWith<F> {
            RepeatWith { repeater }
        }
    }
}

pub mod alloc {
    //  ^^^^^ <- core::alloc

    use super::clone::Clone;
    use super::ptr::NonNull;
    use super::result::Result;

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    // GCCRS: commented out because this lang item not supported yet
    #[lang = "alloc_layout"]
    pub struct Layout {
        size_: usize,
        align_: usize, // NonZeroUsize,
    }
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct AllocError {}

    pub unsafe trait AllocRef {
        fn alloc(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError>;

        fn alloc_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
            self.alloc(layout)
        }

        unsafe fn dealloc(&self, ptr: NonNull<u8>, layout: Layout);

        unsafe fn grow(
            &self,
            ptr: NonNull<u8>,
            old_layout: Layout,
            new_layout: Layout,
        ) -> Result<NonNull<[u8]>, AllocError> {
            self.alloc(new_layout)
        }

        unsafe fn grow_zeroed(
            &self,
            ptr: NonNull<u8>,
            old_layout: Layout,
            new_layout: Layout,
        ) -> Result<NonNull<[u8]>, AllocError> {
            self.alloc_zeroed(new_layout)
        }

        unsafe fn shrink(
            &self,
            ptr: NonNull<u8>,
            old_layout: Layout,
            new_layout: Layout,
        ) -> Result<NonNull<[u8]>, AllocError> {
            self.alloc(new_layout)
        }

        fn by_ref(&self) -> &Self {
            self
        }
    }

    // mod layout
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct LayoutErr {
        private: (),
    }
}

pub mod result {
    //  ^^^^^^ <- core::result

    #[derive(Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
    pub enum Result<T, E> {
        #[lang = "Ok"]
        Ok(T),

        #[lang = "Err"]
        Err(E),
    }
}

pub mod default {
    //  ^^^^^^^ <- core::default

    pub trait Default: super::marker::Sized {
        //    ^^^^^^^ <- derive(Default)
        fn default() -> Self;
    }

    #[inline]
    pub fn default<T: Default>() -> T {
        Default::default()
    }

    #[rustc_builtin_macro]
    #[allow_internal_unstable(core_intrinsics)]
    pub macro Default($item:item) {
        /* compiler built-in */
    }
}

pub mod hash {
    //  ^^^^ <- core::hash

    pub trait Hash {
        //    ^^^^ <- derive(Hash)
        fn hash<H: Hasher>(&self, state: &mut H);

        fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
        where
            Self: super::marker::Sized,
        {
        }
    }
    pub(crate) mod macros {
        #[rustc_builtin_macro]
        #[allow_internal_unstable(core_intrinsics)]
        pub macro Hash($item:item) {
            /* compiler built-in */
        }
    }
    pub use macros::Hash;

    pub trait Hasher {
        fn finish(&self) -> u64;
        fn write(&mut self, bytes: &[u8]);
        #[inline]
        fn write_u8(&mut self, i: u8) {
            // self.write(&[i])
        }
        #[inline]
        fn write_u16(&mut self, i: u16) {
            // self.write(&i.to_ne_bytes())
        }
        #[inline]
        fn write_u32(&mut self, i: u32) {
            // self.write(&i.to_ne_bytes())
        }
        #[inline]
        fn write_u64(&mut self, i: u64) {
            // self.write(&i.to_ne_bytes())
        }
        #[inline]
        fn write_u128(&mut self, i: u128) {
            // self.write(&i.to_ne_bytes())
        }
        #[inline]
        fn write_usize(&mut self, i: usize) {
            // self.write(&i.to_ne_bytes())
        }
        #[inline]
        fn write_i8(&mut self, i: i8) {
            // self.write_u8(i as u8)
        }
        #[inline]
        fn write_i16(&mut self, i: i16) {
            // self.write_u16(i as u16)
        }
        #[inline]
        fn write_i32(&mut self, i: i32) {
            // self.write_u32(i as u32)
        }
        #[inline]
        fn write_i64(&mut self, i: i64) {
            // self.write_u64(i as u64)
        }
        #[inline]
        fn write_i128(&mut self, i: i128) {
            // self.write_u128(i as u128)
        }
        #[inline]
        fn write_isize(&mut self, i: isize) {
            // self.write_usize(i as usize)
        }
    }
}

pub mod option {
    //  ^^^^^^ <- core::option

    use super::clone::Clone;

    #[derive(Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
    pub enum Option<T> {
        #[lang = "None"]
        None,
        #[lang = "Some"]
        Some(T),
    }
    #[derive(Clone, Debug)]
    pub struct IntoIter<A> {
        inner: Item<A>,
    }
    #[derive(Clone, Debug)]
    struct Item<A> {
        opt: Option<A>,
    }
}

pub mod convert {
    //  ^^^^^^^ <- core::convert;
    use super::marker::Sized;
    use super::result::Result;

    pub trait From<T>: Sized {
        #[lang = "from"]
        fn from(_: T) -> Self;
    }
    pub trait TryFrom<T>: Sized {
        type Error;

        fn try_from(value: T) -> Result<Self, Self::Error>;
    }
    pub trait AsRef<T: ?Sized> {
        fn as_ref(&self) -> &T;
    }
    pub trait AsMut<T: ?Sized> {
        fn as_mut(&mut self) -> &mut T;
    }
    #[derive(Copy)]
    pub enum Infallible {}
    impl crate::core::clone::Clone for Infallible {
        fn clone(&self) -> Infallible {
            match *self {}
        }
    }
}

pub mod borrow {
    //  ^^^^^^ <- core::borrow

    use super::marker::Sized;

    pub trait Borrow<Borrowed: ?Sized> {
        fn borrow(&self) -> &Borrowed;
    }
    pub trait BorrowMut<Borrowed: ?Sized>: Borrow<Borrowed> {
        fn borrow_mut(&mut self) -> &mut Borrowed;
    }
}

pub mod any {
    //  ^^^ <- core::any
    use super::clone::Clone;

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
    pub struct TypeId {
        t: u64,
    }

    pub trait Any: 'static {
        fn type_id(&self) -> TypeId;
    }
}

pub mod future {
    //  ^^^^^^ <- core::future
    use super::pin::Pin;
    use super::task::{Context, Poll};

    #[lang = "future_trait"]
    pub trait Future {
        type Output;

        #[lang = "poll"]
        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output>;
    }
}

pub mod pin {
    //  ^^^ <- core::pin
    use super::clone::Clone;

    #[lang = "pin"]
    #[fundamental]
    #[derive(Copy, Clone)]
    pub struct Pin<P> {
        pointer: P,
    }
}

pub mod task {
    //  ^^^^ <- core::pin
    pub use self::poll::Poll;
    pub use self::wake::{Context, RawWaker, RawWakerVTable, Waker};

    use super::clone::Clone;

    pub mod wake {
        use super::Clone;
        #[derive(PartialEq, Copy, Clone, Debug)]
        pub struct RawWakerVTable {
            clone: unsafe fn(*const ()) -> RawWaker,
            wake: unsafe fn(*const ()),
            wake_by_ref: unsafe fn(*const ()),
            drop: unsafe fn(*const ()),
        }
        #[derive(PartialEq, Debug)]
        pub struct RawWaker {
            data: *const (),
            vtable: &'static RawWakerVTable,
        }
        pub struct Waker {
            waker: RawWaker,
        }
        pub struct Context<'a> {
            waker: &'a Waker,
            _marker: crate::core::marker::PhantomData<fn(&'a ()) -> &'a ()>,
        }
    }

    pub mod poll {
        use crate::core::clone::Clone;

        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub enum Poll<T> {
            #[lang = "Ready"]
            Ready(T),

            #[lang = "Pending"]
            Pending,
        }
    }
}

pub mod slice {
    //  ^^^^^ <- core::slice
    pub use index::SliceIndex;
    pub use iter::*;
    pub use raw::*;

    pub mod raw {
        use crate::core::array;
        use crate::core::ptr;

        pub unsafe fn from_raw_parts<'a, T>(data: *const T, len: usize) -> &'a [T] {
            unsafe { &*ptr::slice_from_raw_parts(data, len) }
        }
        pub unsafe fn from_raw_parts_mut<'a, T>(data: *mut T, len: usize) -> &'a mut [T] {
            unsafe { &mut *ptr::slice_from_raw_parts_mut(data, len) }
        }
        pub fn from_ref<T>(s: &T) -> &[T] {
            array::from_ref(s)
        }
        pub fn from_mut<T>(s: &mut T) -> &mut [T] {
            array::from_mut(s)
        }
    }
    pub mod iter {
        use crate::core::clone::Clone;
        use crate::core::marker::PhantomData;
        use crate::core::num::NonZeroUsize;
        use crate::core::ops::function::FnMut;
        use crate::core::ptr::NonNull;

        pub struct Iter<'a, T: 'a> {
            ptr: NonNull<T>,
            end: *const T,
            _marker: PhantomData<&'a T>,
        }
        pub struct IterMut<'a, T: 'a> {
            ptr: NonNull<T>,
            end: *mut T,
            _marker: PhantomData<&'a mut T>,
        }
        // #[derive(Debug)]
        pub struct Windows<'a, T: 'a> {
            v: &'a [T],
            size: NonZeroUsize,
        }
        // #[derive(Debug)]
        pub struct Chunks<'a, T: 'a> {
            v: &'a [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct ChunksMut<'a, T: 'a> {
            v: &'a mut [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct ChunksExact<'a, T: 'a> {
            v: &'a [T],
            rem: &'a [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct ChunksExactMut<'a, T: 'a> {
            v: &'a mut [T],
            rem: &'a mut [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct ArrayChunks<'a, T: 'a, const N: usize> {
            iter: Iter<'a, [T; N]>,
            rem: &'a [T],
        }
        // #[derive(Debug)]
        pub struct ArrayChunksMut<'a, T: 'a, const N: usize> {
            iter: IterMut<'a, [T; N]>,
            rem: &'a mut [T],
        }
        // #[derive(Debug, Clone, Copy)]
        pub struct ArrayWindows<'a, T: 'a, const N: usize> {
            slice_head: *const T,
            num: usize,
            marker: PhantomData<&'a [T; N]>,
        }
        pub struct Split<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            v: &'a [T],
            pred: P,
            finished: bool,
        }
        pub struct SplitMut<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            v: &'a mut [T],
            pred: P,
            finished: bool,
        }
        // #[derive(Debug)]
        pub struct RChunks<'a, T: 'a> {
            v: &'a [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct RChunksExact<'a, T: 'a> {
            v: &'a [T],
            rem: &'a [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct RChunksExactMut<'a, T: 'a> {
            v: &'a mut [T],
            rem: &'a mut [T],
            chunk_size: usize,
        }
        // #[derive(Debug)]
        pub struct RChunksMut<'a, T: 'a> {
            v: &'a mut [T],
            chunk_size: usize,
        }
        pub struct RSplit<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            inner: Split<'a, T, P>,
        }
        pub struct RSplitMut<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            inner: SplitMut<'a, T, P>,
        }
        pub struct RSplitN<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            inner: GenericSplitN<RSplit<'a, T, P>>,
        }
        pub struct RSplitNMut<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            inner: GenericSplitN<RSplitMut<'a, T, P>>,
        }
        pub struct SplitN<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            inner: GenericSplitN<Split<'a, T, P>>,
        }
        pub struct SplitNMut<'a, T: 'a, P>
        where
            P: FnMut(&T) -> bool,
        {
            inner: GenericSplitN<SplitMut<'a, T, P>>,
        }
        // #[derive(Debug)]
        struct GenericSplitN<I> {
            iter: I,
            count: usize,
        }
    }
    pub mod index {
        use crate::core::marker::Sized;
        use crate::core::option::Option;

        pub unsafe trait SliceIndex<T: ?Sized>: private_slice_index::Sealed {
            type Output: ?Sized;

            fn get(self, slice: &T) -> Option<&Self::Output>;

            fn get_mut(self, slice: &mut T) -> Option<&mut Self::Output>;

            unsafe fn get_unchecked(self, slice: *const T) -> *const Self::Output;

            unsafe fn get_unchecked_mut(self, slice: *mut T) -> *mut Self::Output;

            #[track_caller]
            fn index(self, slice: &T) -> &Self::Output;

            #[track_caller]
            fn index_mut(self, slice: &mut T) -> &mut Self::Output;
        }
        mod private_slice_index {
            // use crate::core::ops;
            pub trait Sealed {}

            // impl Sealed for usize {}
            // impl Sealed for ops::Range<usize> {}
            // impl Sealed for ops::RangeTo<usize> {}
            // impl Sealed for ops::RangeFrom<usize> {}
            // impl Sealed for ops::RangeFull {}
            // impl Sealed for ops::RangeInclusive<usize> {}
            // impl Sealed for ops::RangeToInclusive<usize> {}
        }
    }
}

pub mod array {
    //  ^^^^^ <- core::array
    pub fn from_ref<T>(s: &T) -> &[T; 1] {
        unsafe { &*(s as *const T).cast::<[T; 1]>() }
    }

    pub fn from_mut<T>(s: &mut T) -> &mut [T; 1] {
        unsafe { &mut *(s as *mut T).cast::<[T; 1]>() }
    }
    pub mod iter {
        use crate::core::mem::MaybeUninit;
        use crate::core::ops::range::Range;
        pub struct IntoIter<T, const N: usize> {
            data: [MaybeUninit<T>; N],
            alive: Range<usize>,
        }
    }
}

pub mod num {
    //  ^^^ <- core::num
    pub use nonzero::NonZeroUsize;

    pub mod nonzero {
        pub type NonZeroUsize = usize;
    }
}

pub mod unicode {
    //  ^^^^^^^ <- core::unicode;
    pub use unicode_data::conversions;

    pub mod derived_property {
        pub fn Case_Ignorable(c: char) -> bool {
            false
        }
        pub fn Cased(c: char) -> bool {
            false
        }
    }

    pub mod unicode_data {
        pub mod conversions {
            pub fn to_lower(c: char) -> [char; 3] {
                [c, '\0', '\0']
            }
            pub fn to_upper(c: char) -> [char; 3] {
                [c, '\0', '\0']
            }
        }
        pub mod case_ignorable {
            pub fn lookup(c: char) -> bool {
                false
            }
        }
        pub mod cased {
            pub fn lookup(c: char) -> bool {
                false
            }
        }
    }
}

pub mod str {
    //  ^^^ <- core::str;
    pub use convert::*;
    pub use error::*;
    pub use iter::*;
    pub use traits::FromStr;

    pub use iter::Chars;

    use super::option::Option;
    use super::result::Result;
    use crate::core::clone::Clone;

    #[derive(Clone)]
    struct LinesAnyMap;
    struct CharEscapeDebugContinue;
    #[derive(Clone)]
    struct CharEscapeUnicode;
    #[derive(Clone)]
    struct CharEscapeDefault;
    #[derive(Clone)]
    struct IsWhitespace;
    #[derive(Clone)]
    struct IsAsciiWhitespace;
    #[derive(Clone)]
    struct IsNotEmpty;
    #[derive(Clone)]
    struct BytesIsNotEmpty;
    #[derive(Clone)]
    struct UnsafeBytesToStr;

    pub mod pattern {
        use super::Clone;
        use super::Option;

        #[derive(Copy, Clone, Eq, PartialEq, Debug)]
        pub enum SearchStep {
            Match(usize, usize),
            Reject(usize, usize),
            Done,
        }

        pub unsafe trait Searcher<'a> {
            fn haystack(&self) -> &'a str;

            fn next(&mut self) -> SearchStep;

            fn next_match(&mut self) -> Option<(usize, usize)> {
                Option::None
            }

            fn next_reject(&mut self) -> Option<(usize, usize)> {
                Option::None
            }
        }
        pub unsafe trait ReverseSearcher<'a>: Searcher<'a> {
            fn next_back(&mut self) -> SearchStep;

            fn next_match_back(&mut self) -> Option<(usize, usize)> {
                Option::None
            }

            fn next_reject_back(&mut self) -> Option<(usize, usize)> {
                Option::None
            }
        }
        pub trait Pattern<'a>: crate::core::marker::Sized {
            type Searcher: Searcher<'a>;

            fn into_searcher(self, haystack: &'a str) -> Self::Searcher;

            fn is_contained_in(self, haystack: &'a str) -> bool {
                false
            }

            fn is_prefix_of(self, haystack: &'a str) -> bool {
                false
            }

            fn is_suffix_of(self, haystack: &'a str) -> bool
            where
                Self::Searcher: ReverseSearcher<'a>,
            {
                false
            }

            fn strip_prefix_of(self, haystack: &'a str) -> Option<&'a str> {
                Option::None
            }

            fn strip_suffix_of(self, haystack: &'a str) -> Option<&'a str>
            where
                Self::Searcher: ReverseSearcher<'a>,
            {
                Option::None
            }
        }
        pub trait DoubleEndedSearcher<'a>: ReverseSearcher<'a> {}
    }
    pub mod iter {
        use super::Clone;
        use super::{
            BytesIsNotEmpty, CharEscapeDebugContinue, CharEscapeDefault, CharEscapeUnicode,
            IsAsciiWhitespace, IsNotEmpty, IsWhitespace, LinesAnyMap, UnsafeBytesToStr,
        };
        use crate::core::iter::adapters::chain::Chain;
        use crate::core::iter::adapters::flatten::{FlatMap, Flatten};
        use crate::core::iter::adapters::fuse::Fuse;
        use crate::core::iter::adapters::{Copied, Filter, Map};
        use crate::core::slice::iter::Split as SliceSplit;

        #[derive(Clone, Debug)]
        pub struct SplitAsciiWhitespace<'a> {
            pub inner: Map<
                Filter<SliceSplit<'a, u8, IsAsciiWhitespace>, BytesIsNotEmpty>,
                UnsafeBytesToStr,
            >,
        }
        #[derive(Clone)]
        pub struct Chars<'a> {
            pub iter: crate::core::slice::Iter<'a, u8>,
        }
        #[derive(Clone, Debug)]
        pub struct CharIndices<'a> {
            pub front_offset: usize,
            pub iter: Chars<'a>,
        }
        #[derive(Clone)]
        pub struct EncodeUtf16<'a> {
            pub chars: Chars<'a>,
            pub extra: u16,
        }
        #[derive(Clone, Debug)]
        pub struct SplitWhitespace<'a> {
            pub inner: Filter<Split<'a, IsWhitespace>, IsNotEmpty>,
        }
        #[derive(Clone, Debug)]
        pub struct Bytes<'a>(pub Copied<crate::core::slice::Iter<'a, u8>>);
        #[derive(Clone, Debug)]
        pub struct EscapeDebug<'a> {
            pub inner: Chain<
                Flatten<crate::core::option::IntoIter<crate::core::char::EscapeDebug>>,
                FlatMap<Chars<'a>, char::EscapeDebug, CharEscapeDebugContinue>,
            >,
        }
        #[derive(Clone, Debug)]
        pub struct EscapeDefault<'a> {
            pub(super) inner: FlatMap<Chars<'a>, char::EscapeDefault, CharEscapeDefault>,
        }
        #[derive(Clone, Debug)]
        pub struct EscapeUnicode<'a> {
            pub(super) inner: FlatMap<Chars<'a>, char::EscapeUnicode, CharEscapeUnicode>,
        }
        #[derive(Clone, Debug)]
        pub struct Lines<'a>(pub Map<SplitTerminator<'a, char>, LinesAnyMap>);
        #[derive(Clone, Debug)]
        pub struct LinesAny<'a>(pub Lines<'a>);
        // generator_pattern_iterators!
        pub struct MatchIndices;
        pub struct RMatchIndices;
        pub struct Matches;
        pub struct RMatches;
        pub struct Split;
        pub struct RSplit;
        pub struct RSplitN;
        pub struct SplitN;
        pub struct SplitTerminator;
        pub struct RSplitTerminator;
        // generate_pattern_iterators!
    }
    pub mod error {
        use super::Clone;

        #[derive(Copy, Eq, PartialEq, Clone, Debug)]
        pub struct Utf8Error {
            pub valid_up_to: usize,
            pub error_len: super::option::Option<u8>,
        }
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct ParseBoolError {
            pub _priv: (),
        }
    }
    pub mod convert {
        use super::error::Utf8Error;
        use super::Option;
        use super::Result;

        pub fn from_utf8(v: &[u8]) -> Result<&str, Utf8Error> {
            Result::Err(Utf8Error {
                valid_up_to: 0,
                error_len: Option::None,
            })
        }
        pub fn from_utf8_mut(v: &mut [u8]) -> Result<&mut str, Utf8Error> {
            Result::Err(Utf8Error {
                valid_up_to: 0,
                error_len: Option::None,
            })
        }
        pub const unsafe fn from_utf8_unchecked(v: &[u8]) -> &str {
            unsafe { &*(v as *mut [u8] as *mut str) }
        }
        pub unsafe fn from_utf8_unchecked_mut(v: &mut [u8]) -> &mut str {
            unsafe { &mut *(v as *mut [u8] as *mut str) }
        }
    }
    pub mod traits {
        use super::Result;
        pub trait FromStr: crate::core::marker::Sized {
            type Err;

            fn from_str(s: &str) -> Result<Self, Self::Err>;
        }
    }
    pub mod lossy {
        pub struct Utf8Lossy {
            bytes: [u8],
        }
        #[derive(PartialEq, Eq, Debug)]
        pub struct Utf8LossyChunk<'a> {
            pub valid: &'a str,
            pub broken: &'a [u8],
        }
    }
}

pub mod char {
    //  ^^^^ <- core::char
    pub use decode::{decode_utf16, REPLACEMENT_CHARACTER};

    pub mod decode {
        use crate::core::clone::Clone;
        use crate::core::iter::traits::{IntoIterator, Iterator};
        use crate::core::option::Option;

        pub const REPLACEMENT_CHARACTER: char = '\u{FFFD}';

        #[derive(Clone, Debug)]
        pub struct DecodeUtf16<I>
        where
            I: Iterator<Item = u16>,
        {
            iter: I,
            buf: Option<u16>,
        }

        pub fn decode_utf16<I: IntoIterator<Item = u16>>(iter: I) -> DecodeUtf16<I::IntoIter> {
            DecodeUtf16 {
                iter: iter.into_iter(),
                buf: Option::None,
            }
        }
    }
}

pub mod hint {
    //  ^^^^ core::hint
    pub const unsafe fn unreachable_unchecked() -> ! {
        loop {}
    }
    pub fn spin_loop() {}
}

pub mod cell {
    use super::marker::Sized;

    #[repr(transparent)]
    pub struct Cell<T: ?Sized> {
        value: UnsafeCell<T>,
    }

    #[lang = "unsafe_cell"]
    #[repr(transparent)]
    #[repr(no_niche)] // rust-lang/rust#68303.
    pub struct UnsafeCell<T: ?Sized> {
        value: T,
    }
}

pub mod sync {
    pub mod atomic {
        use crate::core::clone::Clone;
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        #[non_exhaustive]
        pub enum Ordering {
            Relaxed,
            Release,
            Acquire,
            AcqRel,
            SeqCst,
        }
        pub fn fence(order: Ordering) {}
    }
}

// ----------  MODULES      END  ----------
