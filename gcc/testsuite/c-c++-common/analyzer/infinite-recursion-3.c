/* { dg-additional-options "-fno-analyzer-call-summaries -Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

struct node
{
  struct node *left;
  struct node *right;
  int val;
};

int sum (struct node *n)
{
  int result = 0;
  if (n->left)
    result += sum (n->left); /* { dg-bogus "infinite recursion" } */
  if (n->right)
    result += sum (n->right); /* { dg-bogus "infinite recursion" } */
  return result;
}
