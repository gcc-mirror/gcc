/* Copyright (C) 2003, 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-Wno-deprecated" } */

/* This tests that the file is only included once
   Neil Booth, 2 August 2003.  */

#include "import2.h"
#import "import2.h"
#include "import2.h"
