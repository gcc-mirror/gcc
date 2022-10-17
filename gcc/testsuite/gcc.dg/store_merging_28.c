/* PR tree-optimization/88709 */
/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fno-tree-vectorize -fno-ipa-icf -fdump-tree-store-merging-details" } */
/* { dg-final { scan-tree-dump-times "New sequence of \[24] stores to replace old one of 16 stores" 8 "store-merging" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-times "New sequence of \[24] stores to replace old one of 6 stores" 1 "store-merging" { target { ! arm*-*-* } } } } */

typedef struct S { char data[16]; } S;
void optimize_me (S);
void optimize_me3 (S, S, S);

void
good ()
{
  optimize_me ((S) { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 });
}

void
bad ()
{
  optimize_me ((S) { 1, 2, 3, 4, 5 });
}

void
why ()
{
  optimize_me ((S) { 1, 2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
}

void
srsly ()
{
  optimize_me3 ((S) { 1, 2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		(S) { 11, 12, 13, 14, 15, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 },
		(S) { 21, 22, 23, 24, 25, 20, 20, 20, 10, 20, 20, 20, 20, 20, 20 });
}

void
srsly_not_one_missing ()
{
  optimize_me3 ((S) { 1, 2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
		(S) { 11, 12, 13, 14, 15, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 },
		(S) { 21, 22, 23, 24, 25, 20, 20, 20, 10, 20, 20, 20, 20, 20, 20, 11 });
}
