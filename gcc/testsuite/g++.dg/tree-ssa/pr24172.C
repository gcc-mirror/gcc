// { dg-options "-O2" }
void IOException( char);
inline void dummy( const char* const mode )
{
  IOException(*mode+*mode);
}

void prepare_inpaint( )
{
  dummy ("rb");
}
