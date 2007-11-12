// Test for PR c++/17577.

/* { dg-do compile } */

#include "pr17577.h"
#pragma implementation "pr17577.h" /* { dg-warning "appears after file" } */
