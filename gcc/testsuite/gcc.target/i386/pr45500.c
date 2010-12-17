/* PR debug/45500 */
/* { dg-do compile } */
/* { dg-options "-g -msse" } */

typedef char V __attribute__ ((__vector_size__ (16)));
static const V s = { '\n', '\r', '?', '\\' };
