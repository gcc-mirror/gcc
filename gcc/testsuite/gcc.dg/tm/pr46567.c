__attribute__((transaction_callable))
static void SeqfileGetLine ()
{
  SSIGetFilePosition ();
}

__attribute__((transaction_callable))
static void readLoop (int addfirst)
{
  if (!addfirst)
    {
      if (!addfirst)
	{
	  SSIGetFilePosition ();
	}
      SeqfileGetLine ();
    }
}
