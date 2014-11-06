/* Limit this to known non-strict alignment targets.  */
/* { dg-do run { target { i?86-*-linux* x86_64-*-linux* } } } */
/* { dg-options "-fsanitize=null,alignment" } */

#include "align-2.c"

/* { dg-output "\.c:(14|15):\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:16:\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'long long int', which requires \[48] byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:(13|16):\[0-9]*: \[^\n\r]*store to misaligned address 0x\[0-9a-fA-F]* for type 'int', which requires 4 byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:23:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:(29|30):\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:30:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:31:\[0-9]*: \[^\n\r]*member access within misaligned address 0x\[0-9a-fA-F]* for type 'struct S', which requires \[48] byte alignment.*" } */
/* { dg-output "\[^\n\r]*\.c:37:\[0-9]*: \[^\n\r]*load of misaligned address 0x\[0-9a-fA-F]* for type 'long long int', which requires \[48] byte alignment" } */
