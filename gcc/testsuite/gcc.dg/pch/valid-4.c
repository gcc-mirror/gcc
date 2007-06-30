/* { dg-options "-I. -Winvalid-pch -Wtrigraphs" } */

#include "valid-4.h"

char * x = "??/";  /* { dg-warning "trigraph" } */

