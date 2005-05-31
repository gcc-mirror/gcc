// { dg-do assemble { target i?86-*-* x86_64-*-* } }
// Origin: Anthony Green  <green@cygnus.com>

void foo ()
{ 
  register const char *h asm("%esi") = "hey"; 
}
