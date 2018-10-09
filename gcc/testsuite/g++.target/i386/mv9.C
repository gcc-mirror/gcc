// { dg-do compile }
// { dg-options "" }   

void foo ();
void foo () __attribute__((target ("sse4")));
void foo () __attribute__((target ("default"))); // { dg-message "previous declaration" }
void foo ()	// { dg-error "attribute for multi-versioned" }
{
}
