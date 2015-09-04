/* { dg-do compile } */

/* This testcase exposes PR65210. Usage of the io_low attribute
   causes assertion failure because code only looks for the io
   attribute if SYMBOL_FLAG_IO is set. */

volatile char q __attribute__((io_low,address(0x81)));
