/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include "../../objc-obj-c++-shared/runtime.h"
#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#endif

/* The following are correct.  */
id test_valid1 = @"test";
id test_valid2 = @"te" @"st";
id test_valid3 = @"te" @"s" @"t";
id test_valid4 = @ "t" @ "e" @ "s" @ "t";

/* The following are accepted too; you can concat an ObjC string to a
   C string, the result being an ObjC string.  */
id test_valid5 = @"te" "st";
id test_valid6 = @"te" "s" @"t";
id test_valid7 = @"te" @"s" "t";

/* The following are not correct.  */
id test_invalid1          = @@"test";            /* { dg-error "stray .@. in program" } */
const char *test_invalid2 = "test"@;             /* { dg-error "stray .@. in program" } */
const char *test_invalid3 = "test"@@;            /* { dg-error "stray .@. in program" } */
const char *test_invalid4 = "te" @"st";          /* { dg-error "expected" } */
id test_invalid5          = @"te" @@"st";        /* { dg-error "repeated .@. before Objective-C string" } */
id test_invalid6          = @@"te" @"st";        /* { dg-error "stray .@. in program" } */
id test_invalid7          = @"te" @"s" @@"t";    /* { dg-error "repeated .@. before Objective-C string" } */
id test_invalid8          = @"te" @@"s" @"t";    /* { dg-error "repeated .@. before Objective-C string" } */
id test_invalid9          = @"te" @"s" @"t" @;   /* { dg-error "stray .@. in program" } */
id test_invalidA          = @"te" @ st;          /* { dg-error "stray .@. in program" } */
                                                 /* { dg-error "expected" "" { target *-*-* } 31 } */
