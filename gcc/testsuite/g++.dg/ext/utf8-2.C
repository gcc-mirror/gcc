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

const u8_char_t      t0[0]   = u8"ab";	// { dg-error "initializer-string for 'const u8_char_t \\\[0]' {aka 'const \(char|char8_t\) \\\[0]'} is too long" }
const u8_char_t      t1[1]   = u8"ab";	// { dg-error "initializer-string for 'const u8_char_t \\\[1]' {aka 'const \(char|char8_t\) \\\[1]'} is too long" }
const u8_char_t      t2[2]   = u8"ab";	// { dg-error "initializer-string for 'const u8_char_t \\\[2]' {aka 'const \(char|char8_t\) \\\[2]'} is too long" }
const u8_char_t      t3[3]   = u8"ab";
const u8_char_t      t4[4]   = u8"ab";

const u8_char_t      u0[0]   = u8"\u2160.";	// { dg-error "initializer-string for 'const u8_char_t \\\[0]' {aka 'const \(char|char8_t\) \\\[0]'} is too long" }
const u8_char_t      u1[1]   = u8"\u2160.";	// { dg-error "initializer-string for 'const u8_char_t \\\[1]' {aka 'const \(char|char8_t\) \\\[1]'} is too long" }
const u8_char_t      u2[2]   = u8"\u2160.";	// { dg-error "initializer-string for 'const u8_char_t \\\[2]' {aka 'const \(char|char8_t\) \\\[2]'} is too long" }
const u8_char_t      u3[3]   = u8"\u2160.";	// { dg-error "initializer-string for 'const u8_char_t \\\[3]' {aka 'const \(char|char8_t\) \\\[3]'} is too long" }
const u8_char_t      u4[4]   = u8"\u2160.";	// { dg-error "initializer-string for 'const u8_char_t \\\[4]' {aka 'const \(char|char8_t\) \\\[4]'} is too long" }
const u8_char_t      u5[5]   = u8"\u2160.";
const u8_char_t      u6[6]   = u8"\u2160.";
