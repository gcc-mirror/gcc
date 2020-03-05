/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-options "-fdump-tree-optimized-blocks" } */

unsigned register1;
unsigned register2;

void busy_wait_for_register (int x)
{
  volatile unsigned* ptr;
  switch(x) {
    case 0x1111:
    ptr = &register1;
    break;

    case 0x2222:
    ptr = &register2;
    break;

    default:
    return;
  }
  while (*ptr) {}
}

/* { dg-final { scan-tree-dump "loop depth 1" "optimized" } } */
