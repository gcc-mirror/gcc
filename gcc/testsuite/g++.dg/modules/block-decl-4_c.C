// PR c++/123627
// { dg-additional-options "-fmodules -fno-module-lazy -fdump-lang-module-alias" }

module m;
import :part;

// { dg-final { scan-lang-dump-times {Read:-[0-9]*'s attached merge key \(matched\) type_decl} 5 module } }
// { dg-final { scan-lang-dump-times {Read:-[0-9]*'s local type merge key \(matched\) type_decl} 5 module } }
