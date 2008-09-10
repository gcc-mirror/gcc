/* This used to crash IRA with -O3 -fPIC.
  See PR 37333.  */
struct yy_buffer_state  {
  int yy_is_interactive;
};
static struct yy_buffer_state * * yy_buffer_stack = 0;
static int yy_n_chars;
int orafce_sql_yyleng;
unsigned char *yy_c_buf_p = 0;
extern char *orafce_sql_yytext;
static const int yy_ec[256] = { };
short yy_accept[155], yy_base[193] = { }, yy_def[193] = { };
short yy_chk[738] = { };
unsigned char *yy_last_accepting_cpos;
int orafce_sql_yylex (int a)
{
  register int yy_current_state;
  unsigned char *yy_cp, *yy_bp;
  register int yy_act;
  while ( 1 )   {
    do {
      char yy_c = yy_ec[*yy_cp];
      if ( yy_accept[yy_current_state] )
	yy_last_accepting_cpos = yy_cp;
      while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
	yy_current_state = yy_def[yy_current_state];
    } while ( yy_current_state != 154 );
yy_find_action:
    yy_act = yy_accept[yy_current_state];
    *yy_cp = '\0';
    switch ( yy_act )  {
      case 2:
	*yy_cp = 1;
	return 265;
      case 24: 
      case 25:
	addlit(orafce_sql_yytext, orafce_sql_yyleng);
      case 26:
	addlit(orafce_sql_yytext, orafce_sql_yyleng);
      case 53:
	yy_fatal_error( "flex scanner jammed" );
	break;
      case 54:
	yy_cp = ++(yy_c_buf_p);
	unsigned long n = 0;
	if ( yy_buffer_stack[0]->yy_is_interactive )
	  for ( ; _IO_getc () != (-1) ;   ++n )
	    yy_n_chars = n;
	if (a == 0)
	{
	  yy_current_state = yy_get_previous_state( );
	  goto yy_find_action;
	}
    }
  }
}
