// { dg-additional-options {-fmodules-ts -fdump-lang-module-alias} }
import "pr99425-1_a.H";
import "pr99425-1_b.H";

void frob (Cont parm)
{
  ssize (parm);
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(new\) template_decl:'::template ssize'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) template_decl:'::template ssize'} module } }
