/* { dg-xfail-if "PR target/60602" { sparc*-*-solaris2.9* && { ! gas } } { "-O0" } } */

struct var_len
{
  int field1;
  const char field2[];
};

/* Note - strictly speaking this array declaration is illegal
   since each element has a variable length.  GCC allows it
   (for the moment) because it is used in existing code, such
   as glibc.  */
static const struct var_len var_array[] = 
{
  { 1, "Long exposure noise reduction" },
  { 2, "Shutter/AE lock buttons" },
  { 3, "Mirror lockup" }
};
