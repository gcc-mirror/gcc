/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests that "#include MACRO" works.  */

/* Source: Neil Booth, 29 Oct 2000.  */

#define MACRO "mi1c.h"
#include MACRO
#ifndef CPP_MIC_H
#error #include MACRO does not work
#endif
