// Test that char8_t does not alias with other types when -fchar8_t is enabled.
// { dg-do compile }
// { dg-options "-fstrict-aliasing -Wstrict-aliasing=1 -fchar8_t" }

extern long l;
char8_t* f() {
  return (char8_t*)&l; // { dg-warning "dereferencing type-punned pointer might break strict-aliasing rules" "char8_t" }
}
