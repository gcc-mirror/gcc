// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101657
// { dg-do compile }
// { dg-additional-options "-H" }

void fun101657()
{
    fail; // { dg-error "undefined identifier 'fail'" }
}

// { dg-final { if ![file exists pr101657.di] \{                } }
// { dg-final {     pass "gdc.dg/pr101657.d   (file exists pr101657.di)" } }
// { dg-final { \} else \{                                      } }
// { dg-final {     fail "gdc.dg/pr101657.d   (file exists pr101657.di)" } }
// { dg-final { \}                                              } }
