void make_file_symbol_completion_list (char *);
/* This tests to make sure PRE doesn't choose the wrong name when
   inserting phi nodes.  Otherwise, we get uses that aren't dominated
   by defs.  
   PR 13177.  */
void location_completer (char *text)
{
	char *p, *symbol_start = text;
	for (p = text; *p != '\0'; ++p) {
		if (*p == '\\' && p[1] == '\'')
			p++;
		else if (*p == ':')
			symbol_start = p + 1;
		else 
			symbol_start = p + 1;
		make_file_symbol_completion_list(symbol_start);
	}
}


