// { dg-options "-w" }
// taken from https://github.com/rust-lang/rust/blob/e1884a8e3c3e813aada8254edfa120e85bf5ffca/library/core/src/cmp.rs#L98

#[lang = "sized"]
pub trait Sized {}

#[lang = "eq"]
#[stable(feature = "rust1", since = "1.0.0")]
#[doc(alias = "==")]
#[doc(alias = "!=")]
pub trait PartialEq<Rhs: ?Sized = Self> {
    /// This method tests for `self` and `other` values to be equal, and is used
    /// by `==`.
    #[must_use]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn eq(&self, other: &Rhs) -> bool;

    /// This method tests for `!=`.
    #[inline]
    #[must_use]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

enum BookFormat {
    Paperback,
    Hardback,
    Ebook,
}

impl PartialEq<BookFormat> for BookFormat {
    fn eq(&self, other: &BookFormat) -> bool {
        self == other
    }
}

pub struct Book {
    isbn: i32,
    format: BookFormat,
}

// Implement <Book> == <BookFormat> comparisons
impl PartialEq<BookFormat> for Book {
    fn eq(&self, other: &BookFormat) -> bool {
        self.format == *other
    }
}

// Implement <BookFormat> == <Book> comparisons
impl PartialEq<Book> for BookFormat {
    fn eq(&self, other: &Book) -> bool {
        *self == other.format
    }
}

// Implement <Book> == <Book> comparisons
impl PartialEq<Book> for Book {
    fn eq(&self, other: &Book) -> bool {
        self.isbn == other.isbn
    }
}

pub fn main() {
    let b1 = Book {
        isbn: 1,
        format: BookFormat::Paperback,
    };
    let b2 = Book {
        isbn: 2,
        format: BookFormat::Paperback,
    };

    let _c1: bool = b1 == BookFormat::Paperback;
    let _c2: bool = BookFormat::Paperback == b2;
    let _c3: bool = b1 != b2;
}
