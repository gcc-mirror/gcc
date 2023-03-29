// PR c++/106656 - P2513 - char8_t Compatibility and Portability Fixes
// { dg-do compile { target c++20 } }

const char *p1 = u8""; // { dg-error "invalid conversion" }
const unsigned char *p2 = u8""; // { dg-error "invalid conversion" }
const signed char *p3 = u8""; // { dg-error "invalid conversion" }
const char *p4 = { u8"" }; // { dg-error "invalid conversion" }
const unsigned char *p5 = { u8"" }; // { dg-error "invalid conversion" }
const signed char *p6 = { u8"" }; // { dg-error "invalid conversion" }
const char *p7 = static_cast<const char *>(u8""); // { dg-error "invalid" }
const char a1[] = u8"text";
const unsigned char a2[] = u8"";
const signed char a3[] = u8""; // { dg-error "cannot initialize array" }
const char a4[] = { u8"text" };
const unsigned char a5[] = { u8"" };
const signed char a6[] = { u8"" }; // { dg-error "cannot initialize array" }

const char *
resource_id ()
{
  static const char res_id[] = u8"";
  return res_id;
}

const char8_t x[] = "fail"; // { dg-error "cannot initialize array" }

void fn (const char a[]);
void
g ()
{
  fn (u8"z"); // { dg-error "invalid conversion" }
}

char c = u8'c';
unsigned char uc = u8'c';
signed char sc = u8'c';
char8_t c8 = 'c';
