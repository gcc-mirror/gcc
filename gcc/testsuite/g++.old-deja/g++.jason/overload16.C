// { dg-do assemble  }
void f (int);			// { dg-error "" } 
void f (long);			// { dg-error "" } 
int main()
{
  f (1 & 0xffffff00UL);		// { dg-error "" } ambiguous
}
