unsigned char	TIFFFax2DMode[20][256];
unsigned char	TIFFFax2DNextState[20][256];
unsigned char	TIFFFaxUncompAction[20][256];
unsigned char	TIFFFaxUncompNextState[20][256];
unsigned char	TIFFFax1DAction[230][256];
unsigned char	TIFFFax1DNextState[230][256];

typedef struct tableentry {
    unsigned short length;
    unsigned short code;
    short       runlen;
} tableentry;

extern tableentry TIFFFaxWhiteCodes[];
extern tableentry TIFFFaxBlackCodes[];

static short sp_data, sp_bit;

static unsigned char
fetchByte (inbuf)

unsigned char **inbuf;

{
    unsigned char byte = **inbuf;
    (*inbuf)++;
    return (byte);
}

static int
decode_white_run (inbuf)

unsigned char **inbuf;

{
    short state = sp_bit;
    short action;
    int runlen = 0;

    for (;;)
    {
	if (sp_bit == 0)
	{
	nextbyte:
	    sp_data = fetchByte (inbuf);
	}

	action = TIFFFax1DAction[state][sp_data];
	state = TIFFFax1DNextState[state][sp_data];
	if (action == 0 )
	    goto nextbyte;
	if (action == 1 )
	    return (-1 );
	if (action == 210 )
	    return (-3 );
	sp_bit = state;
	action = (TIFFFaxWhiteCodes[ action - 2  ].runlen) ;
	runlen += action;
	if (action < 64)
	    return (runlen);
    }
}

