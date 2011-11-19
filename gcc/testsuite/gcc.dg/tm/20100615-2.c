/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

__attribute__((transaction_safe))
void Info_RemoveKey (char *s)
{
	char    *o = 0;
	while (1)
	{
		s++;
		while (*s)
		{
			if (!*s)
				return;
			*o++ = *s++;
		}
		*o = 0;
	}
}
