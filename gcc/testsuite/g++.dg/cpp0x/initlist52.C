// PR c++/45378
// { dg-do compile { target c++11 } }

int main()
{
   int x { 22.2 };		// { dg-error "narrowing" }
}
