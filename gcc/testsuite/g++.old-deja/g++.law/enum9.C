// Special g++ Options: -fshort-enums
// GROUPS passed enums
  extern "C" int printf (const char *, ...);

  enum E { A = 0x80000000, B = 0 };

  main()
  {
    if (sizeof (E) != 4)
	{ printf ("FAIL\n"); return 1; }
    else
	printf ("PASS\n");
    return 0;
  }
