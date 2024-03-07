/* { dg-do compile } */
/* { dg-options "-O2 -minline-memops-threshold=256" } */

char buf[512];

void
mov_big (void)
{
  __builtin_memmove (buf, buf + 12, 354); /* { dg-error "too many bytes" } */
}

