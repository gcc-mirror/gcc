static int parse (int, int);
static int parseStmt (int, int);

ejEval()
{
	int		state;
	state = parse(8  , 0x1 );
}
static int parse(int state, int flags)
{
	switch (state) {
	case 8 :
	case 18 :
	case 6 :
	case 2 :
		state = parseStmt(state, flags);
		break;
	}
}
static int parseStmt(int state, int flags)
{
  parse (2, flags);
}
