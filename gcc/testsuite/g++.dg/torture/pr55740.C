// { dg-do compile }

static bool st_IsPathDelimiter( char c ) { return c == '/'; }
bool IsValidPath( char const * filename )
{
  if ( !filename || filename[0] == 0 )     
    return false;
  char const * run = filename;
  while ( run && *run )       
    {
      if ( run[0] == '.' )   
	if ( run[1] != '.' || ( !st_IsPathDelimiter( run[2] ) && run[2] != 0 ) )   
	  return false;   
      while ( *run && !st_IsPathDelimiter( *run ) )
	++run;
      if ( *run ) 
	++run;
    }
}	// { dg-warning "control reaches end of non-void function" }
