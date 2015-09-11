// { dg-do assemble  }


class baseClass
{
private:
static int variable;
};

class myClass : public baseClass
{
private:
static int variable;                    // { dg-message "private" }
};

myClass::variable = 0;                  //{ dg-error "" } no type
