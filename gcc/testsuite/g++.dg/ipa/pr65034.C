// PR ipa/65034
// { dg-do compile }
// { dg-options "-g -O2" }

enum B { C };
enum D { E };
struct A { A (B, D) { } };
struct F { unsigned g, h, i, j; } a;

void
foo (unsigned x, unsigned y)
{
  switch (x)
    {
    case 6:
      a.i = y;
      break;
    case 7:
      a.j = y;
      break;
    default:
      A (C, E);
    }
}

void
bar (unsigned x, unsigned y)
{
  switch (x)
    {
    case 6:
      a.i = y;
      break;
    case 7:
      a.j = y;
      break;
    default:
      A (C, E);
    }
}
