/* Copyright (C) 2000, 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-A abc=def -A abc\(ghi\) \"-Aabc = jkl\" -A abc=mno -A -abc=mno -Wno-deprecated" } */

/* Test -A command line syntax.  Source Neil Booth.  31 Oct 2000.  */

#if !#abc (def) || !#abc (ghi) || !#abc (jkl) || #abc(mno)
#error Command line -A assertions
#endif
