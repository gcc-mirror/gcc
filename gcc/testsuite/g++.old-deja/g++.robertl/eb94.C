

class baseClass
{
private:
static int variable;
};

class myClass : public baseClass
{
private:
static int variable;                    // this is intentionally duplicated
};

myClass::variable = 0;                  //ERROR - no type
