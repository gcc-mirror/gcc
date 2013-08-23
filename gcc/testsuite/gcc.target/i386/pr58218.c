/* PR target/58218 */
/* { dg-do assemble { target lp64 } } */
/* { dg-options "-mcmodel=medium" } */

struct { float x[16385]; } a = { { 0.f, } };
