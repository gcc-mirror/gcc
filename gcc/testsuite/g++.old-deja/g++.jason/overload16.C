void f (int);			// ERROR - 
void f (long);			// ERROR - 
main()
{
  f (1 & 0xffffff00UL);		// ERROR - ambiguous
}
