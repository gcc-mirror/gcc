// { dg-do run  }
// { dg-options "-fshort-enums" }

// Origin: Mark Mitchell <mark@codesourcery.com>

// On ARM EABI targets this testcase will cause a warning to be emitted
// whilst EABI attributes are being merged at link time unless
// the --no-enum-size-warning option is passed to the linker.  Whilst the
// enum-size attributes should only be emitted if there are values of
// enum type that can escape the compilation unit, gcc cannot currently
// detect this; if this facility is added then this linker option should
// not be needed.  arm-*-linux* should be a good approximation to
// those platforms where the EABI supplement defines enum values to be
// 32 bits wide.
// { dg-options "-fshort-enums -Wl,--no-enum-size-warning" { target arm*-*-linux* } }

enum E { 
  a = -312
};

E e = a;

int main () {
  if ((int) e != -312)
    return 1;
}
