/* PR c/125935 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -g -dA" } */

bool x;

/* { dg-final { scan-assembler-not "(DW_AT_name: \"_Bool\"|\"_Bool..\"\[^\\r\\n\]*DW_AT_name)" } } */
/* { dg-final { scan-assembler-times "(DW_AT_name: \"bool\"|\"bool..\"\[^\\r\\n\]*DW_AT_name)" 1 } } */
