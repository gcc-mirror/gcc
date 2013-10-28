// PR c++/45378
// { dg-options "-std=c++11 -pedantic-errors" }

int main()
{
   int x { 22.2 };		// { dg-error "narrowing" }
}
