extern int bar(int);

int foo(int x)
{
  return 1 + bar(
	({
		int y;
		switch (x)
		{
		case 0: y = 1; break;
		case 1: y = 2; break;
		case 2: y = 3; break;
		case 3: y = 4; break;
		case 4: y = 5; break;
		case 5: y = 6; break;
		default: y = 7; break;
		}
		y;
	})
     );
}
