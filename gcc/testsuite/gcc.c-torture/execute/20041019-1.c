/* { dg-additional-options "-fpermissive" } */

test_store_ccp (int i)
{
  int *p, a, b, c;

  if (i < 5)
    p = &a;
  else if (i > 8)
    p = &b;
  else
    p = &c;

  *p = 10;
  b = 3;

  /* STORE-CCP was wrongfully propagating 10 into *p.  */
  return *p + 2;
}


test_store_copy_prop (int i)
{
  int *p, a, b, c;

  if (i < 5)
    p = &a;
  else if (i > 8)
    p = &b;
  else
    p = &c;

  *p = i;
  b = i + 1;

  /* STORE-COPY-PROP was wrongfully propagating i into *p.  */
  return *p;
}


main()
{
  int x;
  
  x = test_store_ccp (10);
  if (x == 12)
    abort ();
  
  x = test_store_copy_prop (9);
  if (x == 9)
    abort ();

  return 0;
}
