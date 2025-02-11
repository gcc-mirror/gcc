/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-256 -O2" } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbssds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpbuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwsuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwusds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuud\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpdpwuuds\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpphps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmpsadbw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 a;
volatile __m256h b,c;
volatile __m256i x,y,z;
volatile __m128 a_;
volatile __m128h b_,c_;
volatile __m128i x_,y_,z_;
volatile __mmask16 m16;
volatile __mmask8 m;

void extern
avx10_2_test (void)
{
  x = _mm256_dpbssd_epi32 (x, y, z);
  x = _mm256_mask_dpbssd_epi32 (x, m, y, z);
  x = _mm256_maskz_dpbssd_epi32 (m, x, y, z);

  x_ = _mm_dpbssd_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpbssd_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpbssd_epi32 (m, x_, y_, z_);

  x = _mm256_dpbssds_epi32 (x, y, z);
  x = _mm256_mask_dpbssds_epi32 (x, m, y, z);
  x = _mm256_maskz_dpbssds_epi32 (m, x, y, z);

  x_ = _mm_dpbssds_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpbssds_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpbssds_epi32 (m, x_, y_, z_);

  x = _mm256_dpbsud_epi32 (x, y, z);
  x = _mm256_mask_dpbsud_epi32 (x, m, y, z);
  x = _mm256_maskz_dpbsud_epi32 (m, x, y, z);

  x_ = _mm_dpbsud_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpbsud_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpbsud_epi32 (m, x_, y_, z_);

  x = _mm256_dpbsuds_epi32 (x, y, z);
  x = _mm256_mask_dpbsuds_epi32 (x, m, y, z);
  x = _mm256_maskz_dpbsuds_epi32 (m, x, y, z);

  x_ = _mm_dpbsuds_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpbsuds_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpbsuds_epi32 (m, x_, y_, z_);

  x = _mm256_dpbuud_epi32 (x, y, z);
  x = _mm256_mask_dpbuud_epi32 (x, m, y, z);
  x = _mm256_maskz_dpbuud_epi32 (m, x, y, z);

  x_ = _mm_dpbuud_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpbuud_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpbuud_epi32 (m, x_, y_, z_);

  x = _mm256_dpbuuds_epi32 (x, y, z);
  x = _mm256_mask_dpbuuds_epi32 (x, m, y, z);
  x = _mm256_maskz_dpbuuds_epi32 (m, x, y, z);

  x_ = _mm_dpbuuds_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpbuuds_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpbuuds_epi32 (m, x_, y_, z_);

  x = _mm256_dpwsud_epi32 (x, y, z);
  x = _mm256_mask_dpwsud_epi32 (x, m, y, z);
  x = _mm256_maskz_dpwsud_epi32 (m, x, y, z);

  x_ = _mm_dpwsud_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpwsud_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpwsud_epi32 (m, x_, y_, z_);

  x = _mm256_dpwsuds_epi32 (x, y, z);
  x = _mm256_mask_dpwsuds_epi32 (x, m, y, z);
  x = _mm256_maskz_dpwsuds_epi32 (m, x, y, z);

  x_ = _mm_dpwsuds_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpwsuds_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpwsuds_epi32 (m, x_, y_, z_);

  x = _mm256_dpwusd_epi32 (x, y, z);
  x = _mm256_mask_dpwusd_epi32 (x, m, y, z);
  x = _mm256_maskz_dpwusd_epi32 (m, x, y, z);

  x_ = _mm_dpwusd_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpwusd_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpwusd_epi32 (m, x_, y_, z_);

  x = _mm256_dpwusds_epi32 (x, y, z);
  x = _mm256_mask_dpwusds_epi32 (x, m, y, z);
  x = _mm256_maskz_dpwusds_epi32 (m, x, y, z);

  x_ = _mm_dpwusds_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpwusds_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpwusds_epi32 (m, x_, y_, z_);

  x = _mm256_dpwuud_epi32 (x, y, z);
  x = _mm256_mask_dpwuud_epi32 (x, m, y, z);
  x = _mm256_maskz_dpwuud_epi32 (m, x, y, z);

  x_ = _mm_dpwuud_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpwuud_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpwuud_epi32 (m, x_, y_, z_);

  x = _mm256_dpwuuds_epi32 (x, y, z);
  x = _mm256_mask_dpwuuds_epi32 (x, m, y, z);
  x = _mm256_maskz_dpwuuds_epi32 (m, x, y, z);

  x_ = _mm_dpwuuds_epi32 (x_, y_, z_);
  x_ = _mm_mask_dpwuuds_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_dpwuuds_epi32 (m, x_, y_, z_);

  a = _mm256_dpph_ps (a, b, c);
  a = _mm256_mask_dpph_ps (a, m, b, c);
  a = _mm256_maskz_dpph_ps (m, a, b, c);

  a_ = _mm_dpph_ps (a_, b_, c_);
  a_ = _mm_mask_dpph_ps (a_, m, b_, c_);
  a_ = _mm_maskz_dpph_ps (m, a_, b_, c_);

  x = _mm256_mask_mpsadbw_epu8 (x, m16, y, z, 1);
  x = _mm256_maskz_mpsadbw_epu8 (m16, x, y, 1);
  x_ = _mm_mask_mpsadbw_epu8 (x_, m, y_, z_, 1);
  x_ = _mm_maskz_mpsadbw_epu8 (m, x_, y_, 1);
}
