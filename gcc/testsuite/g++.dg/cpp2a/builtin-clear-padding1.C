// PR tree-optimization/102586
// { dg-do compile }
// { dg-options "-Wno-inaccessible-base" }

struct C0 {};
struct C1 {};
struct C2 : C1, virtual C0 {};
struct C3 : virtual C2, C1 {};
struct C4 : virtual C3, C1 {};
struct C5 : C4 {};
struct C6 { char c; };
struct C7 : virtual C6, virtual C3, C1 {};
struct C8 : C7 {};

void
foo (C0 *c0, C1 *c1, C2 *c2, C3 *c3, C4 *c4, C5 *c5, C6 *c6, C7 *c7, C8 *c8)
{
  __builtin_clear_padding (c0);
  __builtin_clear_padding (c1);
  __builtin_clear_padding (c2);	// { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to a non-trivially-copyable type \\\('C2\\\*'\\\)" }
  __builtin_clear_padding (c3);	// { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to a non-trivially-copyable type \\\('C3\\\*'\\\)" }
  __builtin_clear_padding (c4);	// { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to a non-trivially-copyable type \\\('C4\\\*'\\\)" }
  __builtin_clear_padding (c5);	// { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to a non-trivially-copyable type \\\('C5\\\*'\\\)" }
  __builtin_clear_padding (c6);
  __builtin_clear_padding (c7);	// { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to a non-trivially-copyable type \\\('C7\\\*'\\\)" }
  __builtin_clear_padding (c8);	// { dg-error "argument 1 in call to function '__builtin_clear_padding' has pointer to a non-trivially-copyable type \\\('C8\\\*'\\\)" }
}

void
bar ()
{
  C0 c0;
  C1 c1;
  C2 c2;
  C3 c3;
  C4 c4;
  C5 c5;
  C6 c6;
  C7 c7;
  C8 c8;
  __builtin_clear_padding (&c0);
  __builtin_clear_padding (&c1);
  __builtin_clear_padding (&c2);
  __builtin_clear_padding (&c3);
  __builtin_clear_padding (&c4);
  __builtin_clear_padding (&c5);
  __builtin_clear_padding (&c6);
  __builtin_clear_padding (&c7);
  __builtin_clear_padding (&c8);
}
