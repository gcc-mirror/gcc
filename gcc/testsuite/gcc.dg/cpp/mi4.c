/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Undefining a macro guard and re-including the file used to confuse
   file caching in cppfiles.c, and attempt to open a bad fd.  */

#include "mi1c.h"
#undef CPP_MIC_H
#include "mi1c.h"
