// { dg-do compile { target c++11 } }
// { dg-options "" }

#if __cpp_char8_t
typedef char8_t u8_char_t;
#else
typedef char u8_char_t;
#endif

const u8_char_t	s0[]	= u8"ab";
const char16_t	s1[]	= u8"ab";	// { dg-error "from a string literal with type array of .char." }
const char32_t  s2[]    = u8"ab";	// { dg-error "from a string literal with type array of .char." }
const wchar_t   s3[]    = u8"ab";	// { dg-error "from a string literal with type array of .char." }

const u8_char_t      t0[0]   = u8"ab";	// { dg-error "chars is too long" }
const u8_char_t      t1[1]   = u8"ab";	// { dg-error "chars is too long" }
const u8_char_t      t2[2]   = u8"ab";	// { dg-error "chars is too long" }
const u8_char_t      t3[3]   = u8"ab";
const u8_char_t      t4[4]   = u8"ab";

const u8_char_t      u0[0]   = u8"\u2160.";	// { dg-error "chars is too long" }
const u8_char_t      u1[1]   = u8"\u2160.";	// { dg-error "chars is too long" }
const u8_char_t      u2[2]   = u8"\u2160.";	// { dg-error "chars is too long" }
const u8_char_t      u3[3]   = u8"\u2160.";	// { dg-error "chars is too long" }
const u8_char_t      u4[4]   = u8"\u2160.";	// { dg-error "chars is too long" }
const u8_char_t      u5[5]   = u8"\u2160.";
const u8_char_t      u6[6]   = u8"\u2160.";
