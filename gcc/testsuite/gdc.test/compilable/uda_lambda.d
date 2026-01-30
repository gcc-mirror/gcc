enum UDA;
int fun() @UDA => 7;
static assert(fun() == 7);
