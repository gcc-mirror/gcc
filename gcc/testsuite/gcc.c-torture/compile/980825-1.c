typedef enum { FALSE, TRUE } boolean;
enum _errorTypes { FATAL = 1, WARNING = 2, PERROR = 4 };
typedef struct _optionValues {
    struct _include {		 
	boolean	classNames;	 
	boolean	defines;	 
	boolean	enumerators;	 
    } include;
} optionValues;
extern optionValues	Option;
static void applyTagInclusionList( list )
    const char *const list;
{
    boolean mode = TRUE;	 
    const char *p;
    for (p = list  ;  *p != '\0'  ;  ++p)
	switch (*p)
	{
	    case '=':	 
		clearTagList();
		mode = TRUE;
		break;
	    case '+':	mode = TRUE;	break;	 
	    case '-':	mode = FALSE;	break;	 
	    case 'c':	Option.include.classNames	= mode;		break;
	    case 'd':	Option.include.defines		= mode;		break;
	    case 'e':	Option.include.enumerators	= mode;		break;
	    default: error(FATAL, "-i: Invalid tag option '%c'", *p);	break;
	}
}
