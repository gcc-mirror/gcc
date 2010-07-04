/* { dg-do preprocess } */
/* { dg-options "-std=gnu99" } */

#include <stddef.h>
#include "stddef.h"
#include L"stddef.h"		/* { dg-error "include expects" } */
#include u"stddef.h"		/* { dg-error "include expects" } */
#include U"stddef.h"		/* { dg-error "include expects" } */
#include u8"stddef.h"		/* { dg-error "include expects" } */
#include R"(stddef.h)"		/* { dg-error "include expects" } */
#include LR"(stddef.h)"		/* { dg-error "include expects" } */
#include uR"(stddef.h)"		/* { dg-error "include expects" } */
#include UR"(stddef.h)"		/* { dg-error "include expects" } */
#include u8R"(stddef.h)"	/* { dg-error "include expects" } */
