// { dg-do assemble  }
// { dg-options "-fexceptions" }
// GROUPS passed exceptions
// except file
// Message-Id: <9307071456.AA05275@davinci.hio.hen.nl>
// From: akkersdi@hio.hen.nl
// Subject: exceptions broken
// Date: Wed, 7 Jul 1993 16:56:52 +0200 (MET DST)

class	ball {
public:	int	dummy;
	ball() : dummy(0) { ; }
};

void	pitcher()
{
	throw	ball();
}

void	catcher()
{
	try		// <=== gcc 2.* fails here
	{
		pitcher();
	}
	catch (ball) {
		/* sleepy catcher doing nothing */ ;
	}
}

int	main()
{
	catcher();
}
