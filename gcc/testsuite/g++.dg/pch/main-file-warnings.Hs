#pragma once /* { dg-bogus "in main file" } */
#pragma GCC system_header /* { dg-bogus "outside include file" } */
#include_next <stdint.h> /* { dg-bogus "in primary source file" } */
