
__gshared int x = 7;
__gshared int*[70000] px = &x;

void main()
{
	foreach(p; px)
		assert(p && *p == 7);
}

