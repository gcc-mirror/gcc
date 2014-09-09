int foo()
{
	char c;

 	return (c ^ 30  ) > (c ^ 40 );
/*
  these also get the signal :
 	return (c ^ 30  ) == (c ^ 40 );
 	return ((int)c ^ 30  ) > (c ^ 40 );
  also fails if c is "extern char"

  these are ok :
 	return (c + 30  ) > (c ^ 40 );
 	return (c ^ 30  ) > (c + 40 );
 	return (c ^ 30  ) + (c ^ 40 );
 	return ('a' ^ 30  ) > (c ^ 40 );
 	return (c ^ 40 );
 	return (c ^ 30  ) > (c ^ 40 );
*/
}
