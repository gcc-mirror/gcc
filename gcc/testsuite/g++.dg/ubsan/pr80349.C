// PR sanitizer/80349
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

extern const long long int v;

void
foo ()
{
  (int)((v & 50 | 051UL) << 0) << 0;
}
