//Build don't link:
//Check association of {error} for Koenig lookup
class QString { };
int foo()
{
	QString bar;
	if (bar == __null ) {
    }
}
