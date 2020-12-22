// { dg-additional-options "-fmodules-ts" }
// missing semicolon
import "cpp-5_a.H" // { dg-error "expected" }

int main ()
{
}
