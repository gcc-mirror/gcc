/* { dg-do compile } */

short foo() { return __builtin_rev_crc16_data16(0, 0, 0); }
