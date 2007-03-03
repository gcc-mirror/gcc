// { dg-do compile }
// { dg-options " " }

int f()
{

  else  // { dg-error "'else' without a previous 'if'" }
    {
      return 0;
    }
}
