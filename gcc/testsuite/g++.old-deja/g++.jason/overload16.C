// { dg-do assemble  }
void f (int);			// { dg-message "note" } 
void f (long);			// { dg-message "note" } 
int main()
{
  f (1 & 0xffffff00UL);		// { dg-error "ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 6 }
}
