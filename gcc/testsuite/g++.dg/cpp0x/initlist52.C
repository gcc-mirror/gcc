// PR c++/45378
// { dg-options -std=c++0x }

int main()
{
   int x { 22.2 };		// { dg-error "narrowing" }
}
