/* { dg-do compile } */
/* { dg-options "-O2" } */

class nsIURI;

struct nsCOMPtr
{
  operator nsIURI*() const
  {
    return mRawPtr;
  }

  nsIURI *mRawPtr;
};

void func()
{
  nsCOMPtr u1;
  if (!u1 == !u1)
    return;
}

