	int main(void)
	{
		float reale = 1.0f;
		float oneplus;
		int i;
	
	 	if (sizeof (float) != 4)
		  exit (0);

		for (i = 0; ; i++)
		{
			oneplus = 1.0f + reale;
			if (oneplus == 1.0f)
				break;
			reale=reale/2.0f;
		}
		/* Assumes ieee754 accurate arithmetic above.  */
		if (i != 24)
		  abort ();
		else
		  exit (0);
	}

