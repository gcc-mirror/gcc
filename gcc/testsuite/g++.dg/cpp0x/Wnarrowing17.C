// PR c++/90805 - detect narrowing in case values.
// { dg-do compile { target c++11 } }

void f(int i, char c, unsigned u)
{
  switch (i)
    {
    case 2149056512u:; // { dg-error "narrowing conversion of .2149056512. from .unsigned int. to .int." }
    case (long long int) 1e10:; // { dg-error "narrowing conversion of .10000000000. from .long long int. to .int." }
    // { dg-warning "overflow in conversion" "overflow" { target *-*-* } .-1 }
    }

  switch (c)
    // No narrowing, the adjusted type is int.
    case 300:; // { dg-warning "exceeds maximum value for type" }

  switch (u)
    case -42:; // { dg-error "narrowing conversion of .-42. from .int. to .unsigned int." }
}
