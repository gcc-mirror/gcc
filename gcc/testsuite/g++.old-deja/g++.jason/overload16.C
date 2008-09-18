// { dg-do assemble  }
void f (int);			// { dg-message "candidates" } 
void f (long);			// { dg-message "note" } 
int main()
{
  f (1 & 0xffffff00UL);		// { dg-error "ambiguous" }
}
