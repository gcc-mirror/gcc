// { dg-options "-std=c++0x" }

void f(char i, int j) = delete;	// { dg-message "<deleted>" }
void f(int i, ...);		// { dg-message "void f" }

int main()
{
  f(1,1);			// { dg-error "ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 8 }
}
