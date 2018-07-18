// PR c++/86219

template <int a> struct t;
template <int a>
void f ()
{
   const int b = "";		// { dg-error "conversion" }
   t<b>::c;			// { dg-error "constant" }
   // { dg-prune-output "template argument 1 is invalid" }
}
