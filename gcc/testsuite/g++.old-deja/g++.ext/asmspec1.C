// Build don't link:
// Skip if not target: i?86-*-*
// Origin: Anthony Green  <green@cygnus.com>

void foo ()
{ 
  register const char *h asm("%esi") = "hey"; 
}
