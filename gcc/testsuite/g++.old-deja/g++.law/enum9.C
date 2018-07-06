// { dg-do run  }
// { dg-options "-fshort-enums" }

// On ARM EABI targets this testcase will cause a warning to be emitted
// whilst EABI attributes are being merged at link time unless
// the --no-enum-size-warning option is passed to the linker.  Whilst the
// enum-size attributes should only be emitted if there are values of
// enum type that can escape the compilation unit, gcc cannot currently
// detect this; if this facility is added then this linker option should
// not be needed.
// { dg-options "-fshort-enums -Wl,--no-enum-size-warning" { target arm_eabi } }

// GROUPS passed enums
  extern "C" int printf (const char *, ...);

  enum E { A = 0x80000000, B = 0 };

  int
  main()
  {
    if (sizeof (E) != 4)
	{ printf ("FAIL\n"); return 1; }
    else
	printf ("PASS\n");
    return 0;
  }
