void foo ()
{
  if (1)
    goto foo;
  else
    for (;;)
      {
      foo:
	bar ();
	return;
      }
}
