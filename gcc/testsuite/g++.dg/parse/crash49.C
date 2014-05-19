// PR c++/37552
// { dg-do compile }

extern struct A a[1];	// { dg-message "forward declaration" }

void
foo ()
{
  a[0];			// { dg-error "invalid use of incomplete type" }
}
