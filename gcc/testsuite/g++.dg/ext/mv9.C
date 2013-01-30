// { dg-do compile { target i?86-*-* x86_64-*-* } }    
// { dg-options "" }   

void foo ();
void foo () __attribute__((target ("sse4")));
void foo () __attribute__((target ("default"))); // { dg-error "previous declaration" }
void foo ()	// { dg-error "attribute for multi-versioned" }
{
}
