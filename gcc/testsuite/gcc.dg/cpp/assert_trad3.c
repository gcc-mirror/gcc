/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-A abc=def -A abc\(ghi\) \"-Aabc = jkl\" -A abc=mno -A -abc=mno -traditional" } */

/* Test -A command line syntax.  Source Neil Booth.  31 Oct 2000.  */

#if !#abc (def) || !#abc (ghi) || !#abc (jkl) || #abc(mno)
#error Command line -A assertions
#endif
