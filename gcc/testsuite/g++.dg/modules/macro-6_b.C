// { dg-additional-options "-fdump-lang-module -fmodules-atom" }
// { dg-module-bmi {macro} }

export module macro;
import "macro-6_a.H";

int a;

#ifndef foo
#error bad
#endif

// { dg-final { scan-lang-dump {>Reading macro table "macro-6_a.H"} module } }
