/* PR 17880 */
/* { dg-do compile } */
/* { dg-options "-Wsequence-point" } */

int
foo (int x)
{
  unsigned int a;
  int b;

  b = (a += 5) > a;  /* { dg-warning "undefined" "sequence point warning" } */
  b = (a += 5) + a == 10;  /* { dg-warning "undefined" "sequence point warning" } */
  b = (a -= 5) > a;  /* { dg-warning "undefined" "sequence point warning" } */
  b = (a -= 5) + a == 10;  /* { dg-warning "undefined" "sequence point warning" } */
  b = a-- > a;  /* { dg-warning "undefined" "sequence point warning" } */
  b = a-- + a == 10;  /* { dg-warning "undefined" "sequence point warning" } */
  b = ++a > a;  /* { dg-warning "undefined" "sequence point warning" } */
  b = ++a + a == 10;  /* { dg-warning "undefined" "sequence point warning" } */

  if ((a += 5) > a) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if ((a += 5) + a == 10) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if ((a -= 5) > a) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if ((a -= 5) + a == 10) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if (a-- > a) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if (a-- + a == 10) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if (++a > a) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  if (++a + a == 10) return -1;  /* { dg-warning "undefined" "sequence point warning" } */
  do {} while ((a += 5) > a);  /* { dg-warning "undefined" "sequence point warning" } */
  while ((a += 5) > a);  /* { dg-warning "undefined" "sequence point warning" } */
  for ((a += 5) > a;;);  /* { dg-warning "undefined" "sequence point warning" } */
  for (b = (a += 5) > a;;);  /* { dg-warning "undefined" "sequence point warning" } */
  for (; (a += 5) > a;);  /* { dg-warning "undefined" "sequence point warning" } */
  for (;; b = (a += 5) > a);  /* { dg-warning "undefined" "sequence point warning" } */
  for (;; a++ + a++);  /* { dg-warning "undefined" "sequence point warning" } */
  if (a) a++ - a--;    /* { dg-warning "undefined" "sequence point warning" } */
  ((a +=5) > a) ? a : b; /* { dg-warning "undefined" "sequence point warning" } */
  return (a++ - a--); /* { dg-warning "undefined" "sequence point warning" } */
}

void bar (int i)
{
  int a = i++ - i++; /* { dg-warning "undefined" "sequence point warning" } */
} 

void baz (int i)
{
  switch (i++ + i++)  /* { dg-warning "undefined" "sequence point warning" } */
    {
    case 1:
      i++ - i++;  /* { dg-warning "undefined" "sequence point warning" } */
    case 2:
      break;
    }
}
