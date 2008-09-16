// PR c++/37531
// { dg-do compile }
// { dg-options "-std=gnu++98" }

void
foo ()
{
  (int[i]) { 0 };	// { dg-error "was not declared in this scope" }
}
