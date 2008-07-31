/* { dg-do compile } */
/* { dg-options "-funswitch-loops" } */

unsigned short status;
void foo (const _Bool flag)
{
  if (status == 2 || status == 7)
    {
      while (status != 2 && (status != 7 || !flag))
	{
	}
    }
}

