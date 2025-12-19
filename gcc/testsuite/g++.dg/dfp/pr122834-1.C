// PR c++/122834
// { dg-do compile { target { c++11 && float128 } } }
// { dg-options "" }
// { dg-add-options float128 }

typedef decltype (0.0DL) A;
typedef _Float128 B;
void bar (A);			// { dg-message "initializing argument 1 of" }

void
foo (B x)
{
  bar (x);			// { dg-warning "with unordered conversion rank" }
}

auto a = 0.0DL + 1.0F128;	// { dg-error "invalid operands to binary \\\+" }
auto b = 1.0F128 + 0.0DL;	// { dg-error "invalid operands to binary \\\+" }
