/* PR preprocessor/96842 */
/* { dg-do preprocess } */
/* { dg-options "-Wall" } */

#include "Wheader-guard-1-1.h"
#include "Wheader-guard-1-2.h"
#include "Wheader-guard-1-3.h"
#include "Wheader-guard-1-4.h"
#include "Wheader-guard-1-5.h"
#include "Wheader-guard-1-6.h"
#include "Wheader-guard-1-7.h"
#define WHEADER_GUARD_8
#include "Wheader-guard-1-8.h"
#include "Wheader-guard-1-9.h"
#include "Wheader-guard-1-10.h"
#include "Wheader-guard-1-11.h"
#include "Wheader-guard-1-12.h"

int i;
