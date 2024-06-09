fn main ()
{
  let _bc = b'\x80';
  let _bs = b"foo\x80bar";

  let _c = '\xef';        // { dg-error "out of range" }
  let _s = "Foo\xEFBar";  // { dg-error "out of range" }

  let _ = b'あ';          // { dg-error " non-ASCII character" }
  let _ = b'🦀';          // { dg-error " non-ASCII character" }
}
