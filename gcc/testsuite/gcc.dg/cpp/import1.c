/* Copyright (C) 2003, 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-Wno-deprecated" } */

/* This tests that our eagerness to apply the multiple include guard
   optimization to the #import doesn't stop us marking the file
   once-only.

   Neil Booth, 2 August 2003.  */

#include "import1.h"
#import "import1.h"
#undef IMPORT1_H
#define BUG
#include "import1.h"
