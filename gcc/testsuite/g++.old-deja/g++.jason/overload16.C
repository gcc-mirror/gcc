void f (int);			// ERROR - 
void f (long);			// ERROR - 
int main()
{
  f (1 & 0xffffff00UL);		// ERROR - ambiguous
}
