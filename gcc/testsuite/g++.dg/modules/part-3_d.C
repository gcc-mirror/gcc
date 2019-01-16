// { dg-additional-options -fmodules-ts }
import foo;

int main ()
{
  int r = frob (75);
  return !(r == -75);
}

// { dg-prune-output "failed to read" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

// { dg-final { scan-lang-dump { Read:-[0-9] unique mergeable decl function_decl:'::frob'} module { xfail *-*-* } } }
// { dg-final { scan-lang-dump {> Read:-[0-9] unique mergeable decl function_decl:'::foo'} module { xfail *-*-* } } }
