// Build don't link: 
// GROUPS passed boolean
int
main()
{
    typedef char Boolean; // Instrinsic.h
    Boolean c = false;
    bool b = true;

    if (!c != !b)
	;
}
