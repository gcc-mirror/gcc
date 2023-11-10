/* PR pch/9471 */
/* PR pch/47857 */
/* Test will fail if any warnings get issued while compiling the header into a PCH.  */
#include "main-file-warnings.H"
#pragma once /* { dg-warning "in main file" } */
#pragma GCC system_header /* { dg-warning "outside include file" } */
#include_next <stdint.h> /* { dg-warning "in primary source file" } */
