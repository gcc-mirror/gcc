/* Copyright (C) 2000-2024 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Source: Bug report of Jonathan Larmour.  Tweaked for test suite by
   Neil Booth, 17 Jan 2000.  */

/* We would not test the buffer->was_skipping variable when skipping,
   meaning that some false directives confused CPP.  */

#if 0
        # foo
        # fee
        # fie
        # foe
        # fum
#elif 0
        # bundy
#endif
