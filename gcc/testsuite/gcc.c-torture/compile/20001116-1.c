int x[60];
char *y = ((char*)&(x[2*8 + 2]) - 8);
int z = (&"Foobar"[1] - &"Foobar"[0]);
