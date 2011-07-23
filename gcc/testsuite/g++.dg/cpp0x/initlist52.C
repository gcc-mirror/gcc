// PR c++/45378
// { dg-options "-std=c++0x -pedantic-errors" }

int main()
{
   int x { 22.2 };		// { dg-error "narrowing" }
}
