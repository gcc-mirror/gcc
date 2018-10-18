// { dg-additional-options -fmodules-atom }

// missing semicolon
import "cpp-5_a.H" // { dg-error "expected" }

int main ()
{
  // Error here to make sure main is parsed
  return; // { dg-error "" }
}
