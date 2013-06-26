/* { dg-do compile } */

typedef struct Node Node;

struct Node
{
  Node *Pred, *Suc;
  Node *SubBestPred;
  Node *SubBestSuc;
};

void
foo (Node *N)
{
  do
    {
      N->SubBestPred = N->Pred;
      N->SubBestSuc = N->Suc;
    }
  while (N = N->Suc);
}
