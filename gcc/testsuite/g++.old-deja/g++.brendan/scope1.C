// Build don't link: 
// GROUPS passed scoping
int f (int i) {
  if (i)
    for (int j = i; j; j--)
      ;
  return j;	// error: j should only be in scope inside the body of `for'// ERROR - .*
} // ERROR - non-void
