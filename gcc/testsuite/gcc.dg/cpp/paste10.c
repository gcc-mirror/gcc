/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "" } */

/* This testcase used to produce a bogus "invalid paste" warning, owing
   to not clearing a PASTE_LEFT flag.  */

#define strcpy(src) __strcpy_small (src)

#define __strcpy_small(src) src

#define tprintf(format, args...) sprintf(format, ## args)

strcpy(tprintf("<%s>", test))
