fn main ()
{
  // Braces are required
  let _cbl = '\u013'; // { dg-error "unicode escape" }
  let _sbl = "\u013"; //{ dg-error "unicode escape" }

  // One to six hex digits
  let _c0 = '\u{}'; // { dg-error "unicode escape" }
  let _c1 = '\u{0}';
  let _c2 = '\u{00}';
  let _c3 = '\u{000}';
  let _c4 = '\u{0000}';
  let _c5 = '\u{00000}';
  let _c6 = '\u{000000}';
  let _c7 = '\u{0000000}'; // { dg-error "unicode escape" }

  let _s0 = "\u{}"; // { dg-error "unicode escape" }
  let _s1 = "\u{0}";
  let _s2 = "\u{00}";
  let _s3 = "\u{000}";
  let _s4 = "\u{0000}";
  let _s5 = "\u{00000}";
  let _s6 = "\u{000000}";
  let _s7 = "\u{0000000}"; // { dg-error "unicode escape" }

  // Underscores OK except for start
  let _c_ = '\u{00___01__0_1_}';
  let _s_ = "\u{00___01__0_1_}";
  let _c__ = '\u{_00__01__0_}'; // { dg-error "unicode escape" }
  let _s__ = "\u{_00__01__0_}"; // { dg-error "unicode escape" }

  // Must be hex chars
  let _chex = '\u{hex}';  // { dg-error "unicode escape" }
  let _shex = '\u{hex}';  // { dg-error "unicode escape" }

  // Only valid from 0x0 to 0xD7FF and from 0xE000 to 0x10FFF
  let _cd7ff = '\u{D7FF}';
  let _sd7ff = "\u{D7FF}";
  let _cd800 = '\u{D800}'; // { dg-error "unicode escape" }
  let _sd800 = "\u{D800}"; // { dg-error "unicode escape" }

  let _cdfff = '\u{DFFF}'; // { dg-error "unicode escape" }
  let _sdfff = "\u{DFFF}"; // { dg-error "unicode escape" }
  let _ce000 = '\u{E000}';
  let _se000 = "\u{E000}";

  let _clast = '\u{10FFFF}';
  let _slast = "\u{10FFFF}";
  let _clast1 = '\u{110000}'; // { dg-error "unicode escape" }
  let _slast1 = "\u{110000}"; // { dg-error "unicode escape" }

  let _cffffff = '\u{FFFFFF}'; // { dg-error "unicode escape" }
  let _sffffff = "\u{FFFFFF}"; // { dg-error "unicode escape" }

  // unicode escapes cannot be used in bytes or byte strings.
  // Except in raw byte strings (where they aren't escapes).
  let _bc = b'\u{000A}'; // { dg-error "unicode escape" }
  let _bs = b"\u{000A}"; // { dg-error "unicode escape" }
  let _rbs = br"\u{000A}";
}
